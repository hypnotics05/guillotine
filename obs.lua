-- sends REC_START and REC_END from within OBS
--

local fname = os.getenv("HOME") .. "/stuff/videos/time"

local function log(message)
  local file = io.open(fname, "a")
  if file then
    file:write(string.format("%s %s\n", message, os.time()))
    file:close()
  else
    print("Error: Failed to open file")
  end
end

local function on_event(event)
  if event == obslua.OBS_FRONTEND_EVENT_RECORDING_STARTED then
    log("REC_START")
  elseif event == obslua.OBS_FRONTEND_EVENT_RECORDING_STOPPED then
    log("REC_END")
  end
end

-- Entry point of obs script
function script_load(_)
  obslua.obs_frontend_add_event_callback(on_event)
end
