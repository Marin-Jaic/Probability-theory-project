g = function(c) {
  return (exp(-0.002 * (c - 500)) / (1 + exp(-0.002 * (c - 500))))
}

f = function(c){
  return (10000 / c)
}

#markov_matrix = append(markov_matrix, 1)

#računanje stacionarne distribucije
get_stacionary_distribution = function(c, n){
  markov_matrix = vector()
  room_amount = n
  
  for(i in 0:room_amount){
    markov_row = vector()
    
    for(j in 0:room_amount){
      current_value = 0
      if(i > j){
        for(k in 0:j){
          current_value = current_value + choose(i, i - j + k) * (1 - g(c)) ** (i - j + k) * g(c) ** (j - k) * dpois(k, f(c))
        }
      } else if (j < room_amount) {
        for(k in 0:i){
          current_value = current_value + choose(i, k) * (1 - g(c)) ** k * g(c) ** (i - k) * dpois(j - i + k, f(c))
        }
      } else {
        for(k in 0:i){
          pois_result = ppois(room_amount - 1 - i + k, f(c), lower.tail = FALSE)
          current_value = current_value + choose(i, k) * (1 - g(c)) ** k * g(c) ** (i - k) * pois_result
        }
      }
      
      markov_row = append(markov_row, current_value)
    }
    markov_matrix = append(markov_matrix, markov_row)
  }
  
  markov_matrix = matrix(markov_matrix, nrow = room_amount + 1, byrow = TRUE)
  #print(markov_matrix)
  
  #tražimo stacionarnu distribuciju potenciranjem
  
  # markov_matrix = markov_matrix %^% 1000
  # #print(markov_matrix)
  # #print(sum(markov_matrix[1, ]))
  # 
  # stacionary_distribution = markov_matrix[1, ]
  # #print(stacionary_distribution)
  
  #tražimo stancionarnu distribuciju sustavom jednadžbi
  system_matrix = t(markov_matrix)
  for(i in 1:(room_amount + 1)){
    for(j in 1:(room_amount + 1)){
      if(i == j){
        system_matrix[i, j] = system_matrix[i, j] - 1
      }
    }
  }
  
  system_matrix[room_amount + 1, ] = rep(1, room_amount + 1)
  right_side = rep(0, room_amount)
  right_side = append(right_side, 1)
  
  solution = solve(system_matrix, right_side)
  return(solution)
}


#Defacto main, računamo najveće očekivanje
room_amount = 100

expected_earnings = vector()
expected_rooms = vector()
prices = vector()

minprice = 275
maxprice = 285

for(i in seq(minprice, maxprice, by=1)){
  # if(i %% 1000 == 0){
  #   print(i / (maxprice - minprice))
  # }
  c = i
  
  stacionary_distribution = get_stacionary_distribution(c, room_amount)
  temp = 0
  
  for(j in 0:room_amount){
    temp = temp + j * stacionary_distribution[j + 1]
  }
  
  expected_rooms = append(expected_rooms, temp)
  expected_earnings = append(expected_earnings, temp * c)
  prices = append(prices, c)
}

index = which.max(expected_earnings)


plot(prices, expected_earnings)
print(paste("Biggest revenue: ", expected_earnings[index]))
print(paste("Average room occupation: ", expected_rooms[index]))
print(paste("Room price: ", prices[index]))
# print(expected_earnings)
