# Generate a list of 400 workers with random salaries and genders
generate_workers <- function(num_workers = 400) {
  workers <- list()
  for (i in 1:num_workers) {
    salary <- sample(5000:30000, 1)  # Random salary between $5000 and $30000
    gender <- sample(c("Male", "Female"), 1)  # Randomly assign gender
    workers[[i]] <- list(salary = salary, gender = gender)
  }
  return(workers)
}

# Function to generate payment slips
generate_payment_slips <- function(workers) {
  payment_slips <- list()
  
  for (worker in workers) {
    tryCatch({
      if (worker$salary > 10000 && worker$salary < 20000) {
        level <- "A1"
      } else if (worker$salary > 7500 && worker$salary < 30000 && worker$gender == "Female") {
        level <- "A5-F"
      } else {
        level <- "Unclassified"  # Fallback for workers who do not meet any criteria
      }
      
      payment_slips <- append(payment_slips, list(list(salary = worker$salary, gender = worker$gender, level = level)))
      
    }, error = function(e) {
      message("Error: ", e$message)
    })
  }
  
  return(payment_slips)
}

# Main execution
workers <- generate_workers()
payment_slips <- generate_payment_slips(workers)

# Display the payment slips
print(payment_slips)
