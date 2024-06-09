let
  pkgs = import <nixpkgs> {};
  rPkgs = with pkgs.rPackages; [ 
    languageserver

    calendR con2aqi
    flexdashboard
    ggplot2 ggrepel
    htmltools httr2
    jsonlite
    leaflet leaflet_extras
    plotly
    reshape2
    scales shiny shinydashboard shinyMobile shinycssloaders
    tidyverse
  ];
in pkgs.mkShell {
  packages = [
    (pkgs.rWrapper.override{packages = rPkgs;})
    (pkgs.rstudioWrapper.override{packages = rPkgs;})
  ];
}
