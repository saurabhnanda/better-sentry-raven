done = function(summary, latency, requests)
  f = io.open("benchmark.csv", "a+")
  
  f:write(
    string.format("'%s',%f,%f,%f,%f,%f,%f,%f,%d,%d,%d,%f\n",
    os.getenv("TEST_NAME"), summary["requests"]/summary["duration"]*1000000,
    latency.min, latency.max, latency.mean, latency.stdev, latency:percentile(50),
    latency:percentile(90), latency:percentile(99),
    summary["duration"], summary["requests"], summary["bytes"])
  )
  local msg = "summary: %s"
  print(msg:format(summary))
end