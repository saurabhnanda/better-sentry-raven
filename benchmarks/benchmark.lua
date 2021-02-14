done = function(summary, latency, requests)
  f = io.open("benchmark.csv", "a+")
  
  f:write(
    string.format(
      "'%s',%f,%f,%f,%f,%f,%f,%f,%d,%d,%d,%f,%d\n",
      os.getenv("TEST_NAME"), summary["requests"]/summary["duration"]*1000000,
      latency.min, latency.max, latency.mean, latency.stdev, latency:percentile(50),
      latency:percentile(90), latency:percentile(99),
      summary["duration"], summary["requests"], summary["bytes"],
      summary["errors"]["connect"] + summary["errors"]["read"] + summary["errors"]["write"] + summary["errors"]["status"] + summary["errors"]["timeout"]
    )
  )
  local msg = "summary: %s"
  print(msg:format(summary))
end