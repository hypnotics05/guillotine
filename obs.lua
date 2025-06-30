-- sends REC_START and REC_END from within OBS
--

local function log(message)
  local file = io.open(os.getenv("HOME") .. "stuff/videos/time", "a")
  if file then
    file:write(string.format("%s %s", message, os.date("%N")))
    file:close()
  else
    print("Error: Failed to open file")
  end
end

local function on_event(event)
  if event == obs.OBS_FRONTEND_EVENT_RECORDING_STARTED then
    log("REC_START")
  elseif event == obs.OBS_FRONTEND_EVENT_RECORDING_STOPPED then
    log("REC_END")
  end
end

-- Entry point of obs script
function script_load(_)
  obs.obs_frontend_add_event_callback(on_event)
end
