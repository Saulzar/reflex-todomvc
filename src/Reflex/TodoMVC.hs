{-# LANGUAGE RecursiveDo, ScopedTypeVariables, FlexibleContexts, TypeFamilies, ConstraintKinds, TemplateHaskell, OverloadedLists #-}
module Reflex.TodoMVC (main) where

import Prelude hiding (mapM, mapM_, all, sequence)

import Control.Monad hiding (mapM, mapM_, forM, forM_, sequence)
import Control.Monad.Trans
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable hiding (for_)
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import Data.FileEmbed
import Control.Concurrent
import qualified Data.Text as T

import Reflex
import Reflex.Html

import Reflex.Host.App

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

data Task
   = Task { taskDescription :: String
          , taskCompleted :: Bool
          }
   deriving (Show, Eq)

-- | Add a new value to a map; automatically choose an unused key
insertNew_ :: (Enum k, Ord k) => v -> Map k v -> Map k v
insertNew_ v m = case Map.maxViewWithKey m of
  Nothing -> Map.singleton (toEnum 0) v
  Just ((k, _), _) -> Map.insert (succ k) v m
  

setAllCompleted :: Bool -> Map k Task -> Map k Task
setAllCompleted allCompleted = fmap (setCompleted allCompleted)

setCompleted :: Bool -> Task -> Task
setCompleted completed t = t { taskCompleted = completed }

setDescription :: String -> Task -> Task
setDescription d t = t { taskDescription = d }

stripDescription :: Task -> Maybe Task
stripDescription t = case T.unpack $ T.strip $ T.pack (taskDescription t) of 
    ""      -> Nothing 
    trimmed -> Just t { taskDescription = trimmed }

initialTasks :: Map Int Task
initialTasks = Map.empty

updateMap :: Ord k => Map k (a -> Maybe a) -> Map k a -> Map k a
updateMap updates items = Map.union unchanged $ Map.mapMaybe id $ Map.intersectionWith ($) updates items where    
    unchanged = Map.difference items updates

--------------------------------------------------------------------------------
-- Filters
--------------------------------------------------------------------------------

-- | Subsets of the task list that can be selected by the user
data Filter
   = All -- ^ All tasks
   | Active -- ^ Uncompleted tasks
   | Completed -- ^ Completed tasks
   deriving (Show, Eq)

-- | Determine whether this Task should be shown when this Filter is in effect
satisfiesFilter :: Filter -> Task -> Bool
satisfiesFilter f = case f of
  All -> const True
  Active -> not . taskCompleted
  Completed -> taskCompleted
  
 

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

main :: IO ()
main = runHtml (withCss $(embedFile "style.css")) todoMVC

todoMVC :: MonadAppHost t m => HtmlT m ()
todoMVC = do
  div_ [class_ -: "todomvc-wrapper", visibility_ -: "hidden"] $ do
    section_ [class_ -: "todoapp"] $ do  
      mainHeader
      rec tasks <- foldDyn ($) initialTasks $ mergeWith (.)
                     [ insertNew_ <$> newTask
                     , listModifyTasks
                     , (Map.filter $ not . taskCompleted) <$ clearCompleted -- Call out the type and purpose of these things
                     ] 
          newTask <- taskEntry
          listModifyTasks <- taskList activeFilter tasks
          (activeFilter, clearCompleted) <- controls tasks
      return ()
    infoFooter

-- | Display the main header
mainHeader :: MonadAppHost t m => HtmlT m ()
mainHeader = h1_ [] $  text "todos"

-- | Display an input field; produce new Tasks when the user creates them
taskEntry :: MonadAppHost t m => HtmlT m (Event t Task)
taskEntry = do 
  header_ [class_ -: "header"] $ do
    -- Create the textbox; it will be cleared whenever the user presses enter
    rec let newValueEntered = ffilter (==keycodeEnter) (keypress descriptionBox)
    
        postBuild <- lift getPostBuild
        descriptionBox <- textInput [class_ -: "new-todo", placeholder_ -: "What needs to be done?", name_ -: "newTodo"]
                                    $ def & setValue .~ ("" <$ newValueEntered)
                                          & setFocus .~ leftmost [True <$ newValueEntered, True <$ postBuild]
          
                                          
    -- Request focus on this element when the widget is done being built and after the user enters a new Task

    -- | Get the current value of the textbox whenever the user hits enter                                      
    let newValue = tag (current $ value descriptionBox) newValueEntered

    -- | Strip leading and trailing whitespace from the user's entry, and discard it if nothing remains
    return $ fmapMaybe (\d -> stripDescription $ Task d False) newValue



    
-- | Display the user's Tasks, subject to a Filter; return requested modifications to the Task list
taskList :: (MonadAppHost t m, Ord k, Show k)
         => Dynamic t Filter
         -> Dynamic t (Map k Task)
         -> HtmlT m (Event t (Map k Task -> Map k Task))
taskList activeFilter tasks = section_ [class_ -: "main"] $ do
  -- Create "toggle all" button
  toggleAllState <- mapDyn (all taskCompleted . Map.elems) tasks
  
  hidden <- mapDyn  Map.null tasks
  toggleAll <- checkboxView [class_ -: "toggle-all", name_ -: "toggle", styleHidden ~: hidden]  toggleAllState
  label_ [for_ -: "toggle-all"] $ text "Mark all as complete"
  -- Filter the item list
  visibleTasks <- combineDyn (Map.filter . satisfiesFilter) activeFilter tasks
  -- Hide the item list itself if there are no items
  anyVisible <- mapDyn Map.null visibleTasks
  -- Display the items
  items <- ul_ [class_ -: "todo-list", styleHidden ~: anyVisible] $ list visibleTasks todoItem
  -- Aggregate the changes produced by the elements
  itemChangeEvent <- mapDyn (fmap updateMap . mergeMap) items
  return $ mergeWith (.) [ switch $ current itemChangeEvent
                          -- Change all items' completed state when the toggleAll button is clicked_
                         , setAllCompleted . not <$> tag (current toggleAllState) toggleAll
                         ]

-- | Display an individual todo item
todoItem :: MonadAppHost t m
         => Dynamic t Task
         -> HtmlT m (Event t (Task -> Maybe Task))
todoItem todo = do
  description <- nubDyn <$> mapDyn taskDescription todo
  rec -- Construct the attributes for our element; use 
      isCompleted <- mapDyn (toMaybe "completed" . taskCompleted) todo
      isEditing <- mapDyn (toMaybe "editing") editing'
      
      (editing', changeTodo) <- li_ [class_ ~? isEditing, class_ ~? isCompleted] $ do
        (completeChanged, destroy, startEditing) <- div_ [class_ -: "view"] $ do
          -- Display the todo item's completed status, and allow it to be set
          completed <- nubDyn <$> mapDyn taskCompleted todo
          completedCheckbox <- checkboxView [class_ -: "toggle"] completed
          
          let completeChanged = not <$> tag (current completed) completedCheckbox
          
          -- Display the todo item's name for viewing purposes
          (label, _) <- label' [] $ dynText description
          -- Display the button for deleting the todo item
          (destroy, _) <- button' [class_ -: "destroy"] $ return ()
          return (completeChanged, clicked destroy, clicked label)
        -- Set the current value of the editBox whenever we start editing (it's not visible in non-editing mode)
        
        let setEditValue = tagDyn description $ ffilter id $ updated editing'
            
        afterEdit <- delay 0 startEditing    
        editBox <- textInput [class_ -: "edit", name_ -: "title"] $ 
          def & setValue .~ setEditValue & setFocus .~ (True <$ afterEdit)
          
        let -- Set the todo item's description when the user leaves the textbox or presses enter in it
            newDescription = tag (current $ value editBox) $ leftmost
              [  void $ (ffilter (==keycodeEnter) $ keypress editBox)
              ,  void $ (ffilter not $ updated $ hasFocus editBox)
              ] 
            -- Cancel editing (without changing the item's description) when the user presses escape in the textbox
            cancelEdit = const () <$> (ffilter (==keycodeEscape) $ keydown editBox)
            -- Put together all the ways the todo item can change itself
            changeSelf = mergeWith (>=>) [ fmap (\c -> Just . setCompleted c) completeChanged
                                         , const Nothing <$ destroy
                                         , fmap (\d -> stripDescription . setDescription d) newDescription
                                         ]

        -- Without the delay, the focus doesn't take effect because the element hasn't become unhidden yet; we need to use postGui to ensure that this is threadsafe when built with GTK
        -- Determine the current editing state; initially false, but can be modified by various events
        editing <- holdDyn False $ leftmost [ True  <$ startEditing
                                            , False <$ newDescription
                                            , False <$ cancelEdit
                                            ]
        return (editing, changeSelf)
  -- Return an event that fires whenever we change ourselves
  return changeTodo

  
-- | Display the control footer; return the user's currently-selected filter and an event that fires when the user chooses to clear all completed events
controls :: MonadAppHost t m => Dynamic t (Map k Task) -> HtmlT m (Dynamic t Filter, Event t ())
controls tasks = do
  -- Determine the attributes for the footer; it is invisible when there are no todo items  
  empty <- mapDyn (Map.null) tasks
  footer_ [class_ -: "footer", styleHidden ~: empty] $ do
    -- Compute the number of completed and uncompleted tasks
    (tasksCompleted, tasksLeft) <- splitDyn <=< forDyn tasks $ \m ->
      let completed = Map.size $ Map.filter taskCompleted m
      in (completed, Map.size m - completed)
      
    span_ [class_ -: "todo-count"] $ do      
      strong_ [] $ dynText =<< mapDyn show tasksLeft
      dynText =<< mapDyn (\n -> (if n == 1 then " item" else " items") <> " left") tasksLeft
      
    activeFilter <- ul_ [class_ -: "filters"] $ do
      rec activeFilter <- holdDyn All setFilter
          let filterButton f = li_ [] $ do
                selected <- mapDyn (\af -> toMaybe "selected" (f == af)) activeFilter
                (button, _) <- a' [class_ ~? selected] $ text $ show f
                return $ f <$ clicked button
                
          allButton <- filterButton All
          text " "
          activeButton <- filterButton Active
          text " "
          completedButton <- filterButton Completed
          let setFilter = leftmost [allButton, activeButton, completedButton]
      return activeFilter
    
    noneCompleted <- mapDyn (==0) tasksCompleted
    (clearCompleted, _) <- button' (class_ -: "clear-completed" <> hidden_ ~: noneCompleted) $  do
      dynText =<< mapDyn (\n -> "Clear completed (" <> show n <> ")") tasksCompleted
      
      
    return (activeFilter, clicked clearCompleted)

  
    
-- | Display static information about the application 
infoFooter :: MonadAppHost t m => HtmlT m ()
infoFooter = footer_ [class_ -: "info"] $ do
  p_ [] $ text "Click to edit a todo"
  p_ [] $ do
    text "Written by "
    a_ [href_ -: "https://github.com/ryantrinkle"] $ text "Ryan Trinkle"
    p_ [] $ do
      text "Part of "
      a_ [href_ -: "http://todomvc.com"] $ text "TodoMVC"

      

