coffee_dag <- ggdag::dagify(
  alert ~ coffee + deep_breaths, 
  
  exposure = "coffee",
  outcome = "alert",
  labels = c(
    coffee = "Coffee",
    alert = "Alertness",
    deep_breaths = "Deep Breathing"
  )
)

ggdag::ggdag(coffee_dag, 
             layout = "circle", 
             text = FALSE, use_labels = "label")


coffee_dag <- ggdag::dagify(
  alert ~ coffee + stress, 
  deep_breaths ~ stress,
  exposure = "coffee",
  outcome = "alert",
  labels = c(
    coffee = "Coffee",
    alert = "Alertness",
    deep_breaths = "Deep Breathing",
    stress = "Stress"
  )
)

ggdag::ggdag(coffee_dag, 
             layout = "circle", 
             text = FALSE, use_labels = "label")
