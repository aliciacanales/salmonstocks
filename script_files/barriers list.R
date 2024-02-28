## making the barrier data into a list of dataframes with only cost, stream level, and passability of barrier. Arranged by cost: least cost barrier at the top

barrier_list<- list(alsea = data.frame(barrier_id = seq(1,length.out = nrow(alsea_barriers)),
                                                  cost = alsea_barriers$cost,
                                                  strm_level = alsea_barriers$strm_lev,
                                                  passability = alsea_barriers$pass_score),
                    
                    beaver = data.frame(barrier_id = seq(1,length.out = nrow(beaver_barriers)),
                                              cost = beaver_barriers$cost,
                                              strm_level = beaver_barriers$strm_lev,
                                              passability = beaver_barriers$pass_score),
                    
                    coos = data.frame(barrier_id = seq(1,length.out = nrow(coos_barriers)),
                                           cost = coos_barriers$cost,
                                           strm_level = coos_barriers$strm_lev,
                                           passability = coos_barriers$pass_score),
                    
                    coquille = data.frame(barrier_id = seq(1,length.out = nrow(coquille_barriers)),
                                          cost = coquille_barriers$cost,
                                          strm_level = coquille_barriers$strm_lev,
                                          passability = coquille_barriers$pass_score),
                    
                    floras = data.frame(barrier_id = seq(1,length.out = nrow(floras_barriers)),
                                        cost = floras_barriers$cost,
                                        strm_level = floras_barriers$strm_lev,
                                        passability = floras_barriers$pass_score),
                    
                    lower_umpqua = data.frame(barrier_id = seq(1,length.out = nrow(lower_umpqua_barriers)),
                                              cost = lower_umpqua_barriers$cost,
                                              strm_level = lower_umpqua_barriers$strm_lev,
                                              passability = lower_umpqua_barriers$pass_score),
                    
                    middle_umpqua = data.frame(barrier_id = seq(1,length.out = nrow(middle_umpqua_barriers)),
                                               cost = middle_umpqua_barriers$cost,
                                               strm_level = middle_umpqua_barriers$strm_lev,
                                               passability = middle_umpqua_barriers$pass_score),
                    
                    necanicum = data.frame(barrier_id = seq(1,length.out = nrow(necanicum_barriers)),
                                           cost = necanicum_barriers$cost,
                                           strm_level = necanicum_barriers$strm_lev,
                                           passability = necanicum_barriers$pass_score),
                    
                    nehalem = data.frame(barrier_id = seq(1,length.out = nrow(nehalem_barriers)),
                                         cost = nehalem_barriers$cost,
                                         strm_level = nehalem_barriers$strm_lev,
                                         passability = nehalem_barriers$pass_score),
                    
                    nestucca = data.frame(barrier_id = seq(1,length.out = nrow(nestucca_barriers)),
                                          cost = nestucca_barriers$cost,
                                          strm_level = nestucca_barriers$strm_lev,
                                          passability = nestucca_barriers$pass_score),
                    
                    north_umpqua = data.frame(barrier_id = seq(1,length.out = nrow(north_umpqua_barriers)),
                                              cost = north_umpqua_barriers$cost,
                                              strm_level = north_umpqua_barriers$strm_lev,
                                              passability = north_umpqua_barriers$pass_score),
                    
                    salmon = data.frame(barrier_id = seq(1,length.out = nrow(salmon_barriers)),
                                        cost = salmon_barriers$cost,
                                        strm_level = salmon_barriers$strm_lev,
                                        passability = salmon_barriers$pass_score),
                    
                    siletz = data.frame(barrier_id = seq(1,length.out = nrow(siletz_barriers)),
                                        cost = siletz_barriers$cost,
                                        strm_level = siletz_barriers$strm_lev,
                                        passability = siletz_barriers$pass_score),
                    
                    siltcoos = data.frame(barrier_id = seq(1,length.out = nrow(siltcoos_barriers)),
                                          cost = siltcoos_barriers$cost,
                                          strm_level = siltcoos_barriers$strm_lev,
                                          passability = siltcoos_barriers$pass_score),
                    
                    siuslaw = data.frame(barrier_id = seq(1,length.out = nrow(siuslaw_barriers)),
                                         cost = siuslaw_barriers$cost,
                                         strm_level = siuslaw_barriers$strm_lev,
                                         passability = siuslaw_barriers$pass_score),
                    
                    sixes = data.frame(barrier_id = seq(1,length.out = nrow(sixes_barriers)),
                                       cost = sixes_barriers$cost,
                                       strm_level = sixes_barriers$strm_lev,
                                       passability = sixes_barriers$pass_score),
                    
                    south_umpqua = data.frame(barrier_id = seq(1,length.out = nrow(south_umpqua_barriers)),
                                              cost = south_umpqua_barriers$cost,
                                              strm_level = south_umpqua_barriers$strm_lev,
                                              passability = south_umpqua_barriers$pass_score),
                    
                    tenmile = data.frame(barrier_id = seq(1,length.out = nrow(tenmile_barriers)),
                                         cost = tenmile_barriers$cost,
                                         strm_level = tenmile_barriers$strm_lev,
                                         passability = tenmile_barriers$pass_score),
                    
                    tillamook = data.frame(barrier_id = seq(1,length.out = nrow(tillamook_barriers)),
                                           cost = tillamook_barriers$cost,
                                           strm_level = tillamook_barriers$strm_lev,
                                           passability = tillamook_barriers$pass_score),
                    
                    yaquina = data.frame(barrier_id = seq(1,length.out = nrow(yaquina_barriers)),
                                         cost = yaquina_barriers$cost,
                                         strm_level = yaquina_barriers$strm_lev,
                                         passability = yaquina_barriers$pass_score)
                    )

