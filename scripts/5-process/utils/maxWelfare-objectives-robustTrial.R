equality_func = function(x){
  x[1] + x[2] + x[3]
}

objective_func_trial0 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 863 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}

objective_func_trial1 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 753 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}

objective_func_trial2 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 774 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}

objective_func_trial3 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 849 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}

objective_func_trial4 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 733 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}

objective_func_trial5 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 820 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}

objective_func_trial6 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 796 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}

objective_func_trial7 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 677 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}

objective_func_trial8 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 598 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}

objective_func_trial9 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 680 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}

objective_func_trial10 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 847 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}

objective_func_trial11 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 643 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}

objective_func_trial12 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 897 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}

objective_func_trial13 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 635 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}

objective_func_trial14 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 776 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}

objective_func_trial15 = function(x){
  # West historical:
  # minDataFlow: 104, maxDataFlow: 228
  westFlow = x[1]
  west_minDataFlow = 104
  west_maxDataFlow = 228
  westPickup = 22.21787 + 0.65667097*westFlow  
  if (westFlow > west_maxDataFlow){
    westPickup = 22.21787 + 0.65667097*west_maxDataFlow
  }
  if (westFlow < westPickup){
    westPickup = westFlow
  }
  
  # North historical:
  # minDataFlow: 226, maxDataFlow: 414
  northFlow = x[2]
  north_minDataFlow = 226
  north_maxDataFlow = 414
  northPickup = 88.47278 + 0.43976106*northFlow
  if (northFlow > north_maxDataFlow){
    northPickup = 88.47278 + 0.43976106*north_maxDataFlow
  }
  if (northFlow < northPickup){
    northPickup = northFlow
  }
  
  # East historical:
  # minDataFlow: 205, maxDataFlow: 380
  eastFlow = x[3]
  east_minDataFlow = 205
  east_maxDataFlow = 380
  eastPickup = 142.05783 + 0.05451053*eastFlow
  if (eastFlow > east_maxDataFlow){
    eastPickup = 142.05783 + 0.05451053*east_maxDataFlow
  }
  if (eastFlow < eastPickup){
    eastPickup = eastFlow
  }
  
  numFailure = 851 - westPickup - northPickup - eastPickup
  
  return(numFailure)
}


objectiveFuncLst = list(objective_func_trial0,
                        objective_func_trial1,
                        objective_func_trial2,
                        objective_func_trial3,
                        objective_func_trial4,
                        objective_func_trial5,
                        objective_func_trial6,
                        objective_func_trial7,
                        objective_func_trial8,
                        objective_func_trial9,
                        objective_func_trial10,
                        objective_func_trial11,
                        objective_func_trial12,
                        objective_func_trial13,
                        objective_func_trial14,
                        objective_func_trial15)