module Integration.TUIWorkflowSpec where

import Control.Monad.State
import Data.Text (Text, pack, unpack)
import Data.Text.IO as TIO
import Domain.FileSynchronization (synchronizeWithTargetFiles)
import Domain.ItemSelection (selectNextItem, selectPreviousItem)
import Domain.ItemsCursor (makeItemsCursor)
import Domain.State (AppState (MkAppState))
import Domain.ValueSelection (selectNextValue, selectPreviousValue)
import Infrastructure.Config (Config (MkConfig), ConfigItem (..), Pattern (MkPattern), TargetValue (MkTargetValue))
import Infrastructure.FsReadIO (runFsReadIO)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec

spec :: Spec
spec = describe "TUI Workflow Integration Tests" $ do
  describe "Complete user workflow simulation" $ do
    it "should start up, navigate, change values, and modify files correctly" $
      withSystemTempDirectory "tui-test" $ \tmpDir -> do
        -- Setup: Create test config file and target file
        let configFile = tmpDir ++ "/test-config.txt"
            targetFile = tmpDir ++ "/target.conf"

        -- Create target file with initial content
        TIO.writeFile targetFile "setting1=initial\nsetting2=value2\n"

        -- Create test configuration
        let testConfig =
              MkConfig
                [ MkConfigItem
                    { title = "Test Setting 1",
                      path = targetFile,
                      matchingPattern = MkPattern "setting1={{value}}",
                      targetValue = MkTargetValue "initial",
                      possibleValues = [MkTargetValue "initial", MkTargetValue "changed", MkTargetValue "final"]
                    },
                  MkConfigItem
                    { title = "Test Setting 2",
                      path = targetFile,
                      matchingPattern = MkPattern "setting2={{value}}",
                      targetValue = MkTargetValue "value2",
                      possibleValues = [MkTargetValue "value2", MkTargetValue "alternative"]
                    }
                ]

        -- Step 1: Simulate startup - synchronize with files
        syncedConfig <- synchronizeWithTargetFiles testConfig

        -- Step 2: Create initial app state (simulates app startup)
        let Just initialCursor = makeItemsCursor (case syncedConfig of MkConfig items -> items)
            initialState = MkAppState initialCursor

        -- Step 3: Simulate user interactions
        result <-
          runTUIWorkflow
            initialState
            [ NavigateDown, -- Move to second item
              ChangeValueNext, -- Change value2 -> alternative
              NavigateUp, -- Move back to first item
              ChangeValueNext, -- Change initial -> changed
              ChangeValueNext -- Change changed -> final
            ]

        -- Step 4: Verify final file contents
        finalContent <- TIO.readFile targetFile
        finalContent `shouldBe` "setting1=final\nsetting2=alternative\n"

  describe "Navigation workflow" $ do
    it "should handle up/down navigation correctly" $
      withSystemTempDirectory "nav-test" $ \tmpDir -> do
        let targetFile = tmpDir ++ "/nav-target.conf"
        TIO.writeFile targetFile "option1=a\noption2=b\noption3=c\n"

        let testConfig =
              MkConfig
                [ MkConfigItem "Option 1" targetFile (MkPattern "option1={{value}}") (MkTargetValue "a") [MkTargetValue "a", MkTargetValue "x"],
                  MkConfigItem "Option 2" targetFile (MkPattern "option2={{value}}") (MkTargetValue "b") [MkTargetValue "b", MkTargetValue "y"],
                  MkConfigItem "Option 3" targetFile (MkPattern "option3={{value}}") (MkTargetValue "c") [MkTargetValue "c", MkTargetValue "z"]
                ]

        syncedConfig <- synchronizeWithTargetFiles testConfig
        let Just cursor = makeItemsCursor (case syncedConfig of MkConfig items -> items)
            state = MkAppState cursor

        -- Test navigation sequence
        result <- runTUIWorkflow state [NavigateDown, NavigateDown, ChangeValueNext, NavigateUp, NavigateUp, ChangeValueNext]

        finalContent <- TIO.readFile targetFile
        finalContent `shouldBe` "option1=x\noption2=b\noption3=z\n"

-- | Simulate TUI user actions
data TUIAction
  = NavigateUp
  | NavigateDown
  | ChangeValueNext
  | ChangeValuePrev
  deriving (Show, Eq)

-- | Run a sequence of TUI actions and return final state
runTUIWorkflow :: AppState -> [TUIAction] -> IO AppState
runTUIWorkflow initialState actions = do
  (_, finalState) <- runStateT (mapM_ simulateAction actions) initialState
  return finalState
  where
    simulateAction :: TUIAction -> StateT AppState IO ()
    simulateAction NavigateUp = modify selectNextItem
    simulateAction NavigateDown = modify selectPreviousItem
    simulateAction ChangeValueNext = do
      currentState <- get
      newState <- liftIO $ runFsReadIO $ selectNextValue currentState
      put newState
    simulateAction ChangeValuePrev = do
      currentState <- get
      newState <- liftIO $ runFsReadIO $ selectPreviousValue currentState
      put newState
