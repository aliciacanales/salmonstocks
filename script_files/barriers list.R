
barrier_list<- list(alsea = data.frame(barrier_id = seq(1,length.out = nrow(alsea)),
                                                  cost = alsea$cost,
                                                  strm_level = alsea$strm_lev,
                                                  passability = alsea$pass_score),
                    
                    beaver = data.frame(barrier_id = seq(1,length.out = nrow(beaver)),
                                              cost = beaver$cost,
                                              strm_level = beaver$strm_lev,
                                              passability = beaver$pass_score),
                    
                    coos = data.frame(barrier_id = seq(1,length.out = nrow(coos)),
                                           cost = coos$cost,
                                           strm_level = coos$strm_lev,
                                           passability = coos$pass_score),
                    
                    coquille = data.frame(barrier_id = seq(1,length.out = nrow(coquille)),
                                          cost = coquille$cost,
                                          strm_level = coquille$strm_lev,
                                          passability = coquille$pass_score),
                    
                    floras = data.frame(barrier_id = seq(1,length.out = nrow(floras)),
                                        cost = floras$cost,
                                        strm_level = floras$strm_lev,
                                        passability = floras$pass_score),
                    
                    lower_umpqua = data.frame(barrier_id = seq(1,length.out = nrow(lower_umpqua)),
                                              cost = lower_umpqua$cost,
                                              strm_level = lower_umpqua$strm_lev,
                                              passability = lower_umpqua$pass_score),
                    
                    middle_umpqua = data.frame(barrier_id = seq(1,length.out = nrow(middle_umpqua)),
                                               cost = middle_umpqua$cost,
                                               strm_level = middle_umpqua$strm_lev,
                                               passability = middle_umpqua$pass_score),
                    
                    necanicum = data.frame(barrier_id = seq(1,length.out = nrow(necanicum)),
                                           cost = necanicum$cost,
                                           strm_level = necanicum$strm_lev,
                                           passability = necanicum$pass_scores),
                    
                    nehalem = data.frame(barrier_id = seq(1,length.out = nrow(nehalem)),
                                         cost = nehalem$cost,
                                         strm_level = nehalem$strm_lev,
                                         passability = nehalem$pass_score),
                    
                    nestucca = data.frame(barrier_id = seq(1,length.out = nrow(nestucca)),
                                          cost = nestucca$cost,
                                          strm_level = nestucca$strm_lev,
                                          passability = nestucca$pass_score),
                    
                    north_umpqua = data.frame(barrier_id = seq(1,length.out = nrow(north_umpqua)),
                                              cost = north_umpqua$cost,
                                              strm_level = north_umpqua$strm_lev,
                                              passability = north_umpqua$pass_score),
                    
                    salmon = data.frame(barrier_id = seq(1,length.out = nrow(salmon)),
                                        cost = salmon$cost,
                                        strm_level = salmon$strm_lev,
                                        passability = salmon$pass_score),
                    
                    siletz = data.frame(barrier_id = seq(1,length.out = nrow(siletz)),
                                        cost = siletz$cost,
                                        strm_level = siletz$strm_lev,
                                        passability = siletz$pass_score),
                    
                    siltcoos = data.frame(barrier_id = seq(1,length.out = nrow(siltcoos)),
                                          cost = siltcoos$cost,
                                          strm_level = siltcoos$strm_lev,
                                          passability = siltcoos$pass_score),
                    
                    siuslaw = data.frame(barrier_id = seq(1,length.out = nrow(siuslaw)),
                                         cost = siuslaw$cost,
                                         strm_level = siuslaw$strm_lev,
                                         passability = siuslaw$pass_score),
                    
                    sixes = data.frame(barrier_id = seq(1,length.out = nrow(sixes)),
                                       cost = sixes$cost,
                                       strm_level = sixes$strm_lev,
                                       passability = sixes$pass_score),
                    
                    south_umpqua = data.frame(barrier_id = seq(1,length.out = nrow(south_umpqua)),
                                              cost = south_umpqua$cost,
                                              strm_level = south_umpqua$strm_lev,
                                              passability = south_umpqua$pass_score),
                    
                    tenmile = data.frame(barrier_id = seq(1,length.out = nrow(tenmile)),
                                         cost = tenmile$cost,
                                         strm_level = tenmile$strm_lev,
                                         passability = tenmile$pass_score),
                    
                    tillamook = data.frame(barrier_id = seq(1,length.out = nrow(tillamook)),
                                           cost = tillamook$cost,
                                           strm_level = tillamook$strm_lev,
                                           passability = tillamook$pass_score),
                    
                    yaquina = data.frame(barrier_id = seq(1,length.out = nrow(yaquina)),
                                         cost = yaquina$cost,
                                         strm_level = yaquina$strm_lev,
                                         passability = yaquina$pass_score)
                    )