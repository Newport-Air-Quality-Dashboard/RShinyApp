library(leaflet)
library(htmltools)

generate_star_icon <- function(hex_color, size = 20) {
  icon_html <- HTML(sprintf(
    '<div style="
      width: %dpx; 
      height: %dpx; 
      clip-path: polygon(
        50%% 0%%, 
        61%% 35%%, 
        98%% 35%%, 
        68%% 57%%, 
        79%% 91%%, 
        50%% 70%%, 
        21%% 91%%, 
        32%% 57%%, 
        2%% 35%%, 
        39%% 35%%
      ); 
      background-color: %s;">
    </div>', size, size, hex_color))
  return(icon_html)
}

# Example usage:
star_icon <- generate_star_icon("#FF0000")
print(star_icon)
