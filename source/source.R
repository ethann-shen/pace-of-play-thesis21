## Intersections Function


create_intersections <- function(event_data, box, geometry) {
  # LINESTRING objects created from transformed/rescaled coordinates 
  
  if (is.null(event_data %>% pull(geometry))) stop("Please create dataframe with a LINESTRING geometry column.")
  
  intersections = list()
  
  for (i in 1:nrow(event_data)) {
    intersections[[i]] <- box %>% 
      st_intersects(event_data[i,] %>% pull(geometry)) %>% 
      purrr::map_int(length) %>% 
      {which(. > 0)} 
    
    if (i %% 2000 == 1)  print(i)
  }
  return(intersections)
}



## Filling Grids Function


fill_grids <- function(event_data, box, intersection_list, metric) {
  total_speeds = list()
  for (b in 1:nrow(box)){
    total_speeds[[b]] <- c(NA)
  }
  
  for (i in 1:nrow(event_data)) {
    if (i %% 2000 == 1)  print(i)
    for (j in intersection_list[[i]]) {
      total_speeds[[j]] <- c(total_speeds[[j]], event_data %>% tibble::as_tibble() %>% {.[i,]} %>% pull(metric))
    } 
  }
  #agg_total_speeds <- map_dbl(total_speeds, metric, na.rm=TRUE) 
  return(total_speeds)
}

##  Heatmap Function
draw_pitch <- function(plot, pitch_length = 105, pitch_width = 70, color = "#252525") {
  plot + 
    geom_point(x = pitch_length / 2, y = pitch_width / 2, shape = 1, size = 20, color = color) +
    geom_rect(xmin = 0, 
              xmax = 16.5, 
              ymin = (pitch_width - 40.3) / 2, 
              ymax = (pitch_width - 40.3) / 2 + 40.3,
              fill = "white", color = color, alpha = 0) +
    geom_rect(xmin = pitch_length - 16.5, 
              xmax = pitch_length, 
              ymin = (pitch_width - 40.3) / 2, 
              ymax = (pitch_width - 40.3) / 2 + 40.3,
              fill = "white", color = color, alpha = 0) +
    geom_rect(xmin = 0, 
              xmax = 5.5, 
              ymin = (pitch_width - 40.3) / 2 + 11, 
              ymax = (pitch_width - 40.3) / 2 + 40.3 - 11,
              fill = "white", color = color, alpha = 0) +
    geom_rect(xmin = pitch_length - 5.5, 
              xmax = pitch_length, 
              ymin = (pitch_width - 40.3) / 2 + 11, 
              ymax = (pitch_width - 40.3) / 2 + 40.3 - 11,
              fill = "white", color = color, alpha = 0) +
    geom_segment(aes(x = 0, y = 0, xend = pitch_length, yend = 0), color = color) + 
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = pitch_width), color = color) +
    geom_segment(aes(x = pitch_length, y = 0, xend = pitch_length, yend = pitch_width), color = color) +
    geom_segment(aes(x = 0, y = pitch_width, xend = pitch_length, yend = pitch_width), color = color) +
    geom_segment(aes(x = pitch_length / 2, y = 0, xend = pitch_length / 2, yend = pitch_width), color = color) +
    theme_bw() +
    labs(x = "", y = "") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "#cdff9c"), 
          axis.line = element_line(colour = color))
}
create_heatmap <- function(boxes, metric, legend_scale = "sequential", box_outline = FALSE,
                           pitch_length = 105, pitch_width = 70,
                           min_x_coord = 0, max_x_coord = 105, min_y_coord = 0, max_y_coord = 70)  {
  
  if (any(c(min_x_coord, max_x_coord, min_y_coord, max_y_coord) %% 5 != 0)) stop("Coordinates must be multiples of 5. Plese provide a different coordinate value.")  
  if (any(c(min_x_coord, max_x_coord, min_y_coord, max_y_coord) < 0)) stop("Coordinates must be positive.")
  if (any(c(min_x_coord, max_x_coord) > 105)) stop("x-coordinates must be less than 105.")
  if (any(c(min_y_coord, max_y_coord) > 70)) stop("y-coordinates must be less than 70")
  
  start_x <- seq(from = (min_x_coord/5) + 1, to = (min_x_coord/5) + 1 + 273, by= 21)
  end_x <- seq(from = max_x_coord/5, to = max_x_coord/5 + 273, by = 21)
  
  x_dim_boxes <- c()
  for (i in 1:length(start_x)) {
    x_dim_boxes <- c(x_dim_boxes, seq(start_x[i], end_x[i]))
  }
  
  start_y <- (21 * min_y_coord / 5) + 1
  end_y <- (21 * max_y_coord / 5) 
  y_dim_boxes <- seq(start_y, end_y)
  
  p <- (boxes %>% 
          mutate(`Speed (m/s)` = metric) %>% 
          slice(intersect(x_dim_boxes, y_dim_boxes)) %>% 
          ggplot() + 
          geom_sf(aes(fill = `Speed (m/s)`), color = NA)) %>% 
    draw_pitch() + # all the aesthetics of the pitch
    scale_x_continuous(breaks = seq(0, pitch_length, 15), labels = seq(0, pitch_length, 15)) +
    scale_y_continuous(breaks = seq(0, pitch_width, 10), labels = seq(0, pitch_width, 10)) 
  
  rng = range(metric[intersect(x_dim_boxes, y_dim_boxes)], na.rm=TRUE)
  lower <- floor(rng[1])
  upper <- ceiling(rng[2])
  if (legend_scale == "sequential") {
    p <- p + 
      scale_fill_gradient(low = "#ffffb2", high = "#bd0026", na.value = "#cdff9c", 
                          #breaks = seq(lower, upper, (upper - lower)/4), 
                          #limits = c(lower, upper)
      ) 
  } else if (legend_scale == "diverging") {
    p <- p + scale_fill_gradient2(low  = "red", mid = "white", high = "blue", na.value = "#cdff9c"#, breaks = seq(lower, upper, (upper - lower)/4), limits = c(lower, upper)
    ) 
  }
  if (box_outline == TRUE) {
    p <- p + geom_sf(data = boxes, fill=NA, linetype = 2, color = "black")
  }
  return(p)
}

## Filters only possessions with >=3 events from each league

calculate_speeds_df <- function(event_data, poss_events = 3) {
  event_data %>% 
    mutate(rowid = row_number()) %>%
    group_by(match_id, match_period, poss_id) %>% 
    mutate(duration = lead(event_sec) - (event_sec)) %>% # I want the duration and speed of ALL events
    ungroup() %>% 
    filter(!is.na(duration)) %>% 
    mutate(EW_dist = abs(end_x - start_x),
           NS_dist = abs(end_y - start_y),
           total_dist = sqrt(EW_dist^2 + NS_dist^2),
           E_dist = ifelse(end_x - start_x <= 0, NA, end_x - start_x),
           
           EW_speed = if_else(duration == 0, EW_dist, EW_dist / duration),
           NS_speed = if_else(duration == 0, NS_dist, NS_dist / duration),
           total_speed = if_else(duration == 0, total_dist, total_dist / duration),
           E_speed = if_else(duration == 0, E_dist, E_dist / duration)
    ) %>% 
    filter(event_name == "Pass" | sub_event_name %in% c("Free Kick", "Throw in", "Goal kick", "Corner","Free kick cross")) %>% 
    filter(!((end_x == 0 & end_y == 70) | (end_y == 0 & end_x == 105))) %>% 
    group_by(match_id, match_period, poss_id) %>% 
    mutate(num_events = n()) %>%  
    filter(num_events >= poss_events) %>% # default is to keep only possessions with >=3  events (excluding duels, etc)
    ungroup()
}

## Fill Zones

fill_zones <- function(zones, speeds, metric) {
  zone_speeds <- list()
  zone_intersections <- zones %>% st_intersects(grids5x5) 
  
  for (i in 1:length(zone_intersections)) {
    zone_speeds[[i]] <- speeds[zone_intersections[[i]]]
  }
  return(map_dbl(zone_speeds, metric, na.rm=TRUE))
}

perc_change <- function(x, y) {
  (y-x)/x
}

`%notin%` <- Negate(`%in%`)


create_facet_heatmap <- function(aggregate_data, legend_scale = "sequential",
                                 pitch_length = 105, pitch_width = 70, nrow = 2, ncol = 4, color = "#252525") {
  
  num_clubs <- aggregate_data$club %>% unique() %>% length()
  lower <- aggregate_data$value %>% min(na.rm=TRUE) %>% floor()
  upper <- aggregate_data$value %>% max(na.rm=TRUE) %>% ceiling()
  p <- aggregate_data %>% 
    bind_cols(
      purrr::map_dfr(seq_len(num_clubs), ~grids5x5)
    ) %>% 
    ggplot() + 
    geom_sf(aes(geometry = geometry, fill = value), color=NA) + 
    scoutr::fc_annotate_pitch(fill = NA, color ="black") + 
    #scoutr::fc_theme_bw() + 
    facet_wrap(.~club, nrow = nrow, ncol = ncol) + 
    #scale_fill_gradient2(low  = "red", mid = "white", high = "blue", na.value = "#cdff9c") +
    theme_bw() +
    labs(x = "", y = "") +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(fill = "white"),
          axis.line = element_line(colour = color),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) 
  
  if (legend_scale == "sequential") {
    p <- p + 
      scale_fill_gradient(low = "#ffffb2", high = "#bd0026", na.value = "#cdff9c", 
                          
                         
      ) 
  } else if (legend_scale == "diverging") {
    p <- p + scale_fill_gradient2(low  = "blue", mid = "white", high = "red", na.value = "#cdff9c",
                                  breaks = c(-4,-2,0,2,4), 
                                  
    ) 
  }
  
  p
}

