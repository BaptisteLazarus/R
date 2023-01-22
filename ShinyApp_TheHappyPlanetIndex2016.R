library(shiny)
library(tidyverse)
library(shinydashboard)
library(DT)
library(ggplot2)
library(datasets)
library("FactoMineR")
library(leaflet)
library(readr)
library(readxl)
library(png)

data = read_xlsx("hpi-data-2016.xlsx", sheet = "data")
dataPlot = read_xlsx("hpi-data-2016.xlsx", sheet = "dataPlot")
dataCluster = read_xlsx("hpi-data-2016.xlsx", sheet = "Feuil1")

localisation <- data.frame(Ville = c("Chad	n°140", "Luxembourg n°139",	"Togo n°138",	"Benin n°137",	"Mongolia n°136",	"Cote d'Ivoire n°135",	"Turkmenistan n°134",	"Sierra Leone n°133",	"Swaziland n°132",	"Burundi n°131"),
                            Latitude = c(	12.113056,49.611621, 6.131944, 6.497222, 47.886399,6.816667,39.122285,8.484444, -26.5179414, -3.342804),
                            Longitude = c( 15.049167,6.1319346,-1.222778,2.605, 106.905744, -5.283333,59.384377,-13.234444, 31.4629694, 29.366198))

localisation2 <- data.frame(Ville = c("Costa Rica n°1","Mexico n°2","Colombia n°3","Vanuatu n°4","Vietnam n°5","Panama n°6","Nicaragua n°7","Bangladesh n°8","Thailand n°9","Ecuador n°10"),
                           Latitude = c(	9.93,19.43, 4.58, -17.527907, 21.0277644,8.983333,12.8691653, 23.684994, 13.7563309,-1.831239),
                           Longitude = c( -84.09,-99.13,-74.07,168.402026, 105.8341598,-79.516667,-85.1411896, 90.356331, 100.5017651, -78.183406))

carte <- leaflet(localisation) %>% addTiles() %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1,
             radius = 500000, popup = ~paste(Ville),
             color = "red", fillOpacity = 0.5)

carte2 <- leaflet(localisation2) %>% addTiles() %>%
  addCircles(lng = ~Longitude, lat = ~Latitude, weight = 1,
             radius = 500000, popup = ~paste(Ville),
             color = "green", fillOpacity = 0.5)

dataACP=c(data[,4:8],data[,11:13])
res = PCA(dataACP, scale.unit = TRUE) #on ne réprésente pas l'index GINI car beaucoup de valeurs manquantes
plotACP = plot(res, choix="var")

plotRegion = table(data$Region)
barplot(plotRegion, main = "Répartition géographique des pays de l'étude", xlab = "Région du globe", ylab = "Nombre de pays")

#centrage réduction des données
#pour éviter que variables à forte variance pèsent indument sur les résultats
data2 <- c(data[,2],data[,4:13])
data2 = as.data.frame(data2)
data.cr <- scale(data2[,2:11],center=T,scale=T)
#matrice des distances entre individus
d.data <- dist(data.cr)
#CAH - critère de Ward
#method = Â« ward.D2 Â» correspond au vrai critère de Ward
#utilisant le carré de la distance
cah.ward <- hclust(d.data,method="ward.D2")
#affichage dendrogramme
plotCluser = plot(cah.ward)

#dendrogramme avec matérialisation des groupes
rect.hclust(cah.ward,k=2)
#découpage en 2 groupes
groupes.cah <- cutree(cah.ward,k=2)
#liste des groupes
print(sort(groupes.cah))

#(1)évaluer la proportion d'inertie expliquée
inertie.expl <- rep(0,times=10)
for (k in 2:10){
  clus <- kmeans(data.cr,centers=k,nstart=5)
  inertie.expl[k] <- clus$betweenss/clus$totss
}
#graphique
plot(1:10,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie expliquée")
#(2) indice de Calinski Harabasz - utilisation du package fpc
library(fpc)
#évaluation des solutions
sol.kmeans <- kmeansruns(data.cr,krange=2:10,criterion="ch")
#graphique
plotsolution = plot(1:10,sol.kmeans$crit,type="b",xlab="Nb. de groupes",ylab="Silhouette")

data2 = as.data.frame(data)
data2

ui <- dashboardPage(
  dashboardHeader(title = "Indice du bonheur", titleWidth = 300),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Donnees brutes", tabName = "brut", icon = icon("table")),
    menuItem(
      "Description des variables", tabName = "var", icon = icon("list-ol")),
    menuItem(
      "Comparer régions du monde", tabName = "continent", icon = icon("chart-line")),
    menuItem(
      "Carte du monde", tabName = "carte", icon = icon("map")),
    menuItem(
      "Graphique", tabName = "graph", icon = icon("fas fa-chart-bar")),
    menuItem(
      "ACP", tabName = "acp", icon = icon("fas fa-subscript")),
    menuItem(
      "Clusters", tabName = "clusters", icon = icon("fas fa-subscript")))),
  dashboardBody(
    tabItems(
      tabItem("brut", tags$h1("Donnees brutes"),
              datatable(head(data, 140), options = list(
                columnDefs = list(list(className = 'HPI Rank', targets = 5)),
                pageLength = 15, lengthMenu = c(10, 20, 30, 50)))),
      tabItem("var",style="overflow-y : scroll;overflow-x: scroll;",  tags$h1("Description des variables"),
              "Présentation des différentes variables.",
              fluidPage(navlistPanel(
                tabPanel("Région","Variable Qualitative",verbatimTextOutput("sum_region") ,box(title = "Diagramme des régions", solidHeader = T, status = "primary",plotOutput("plotRegion"), width = 12)),
                tabPanel("Average Life ","Variable Quantitative", verbatimTextOutput("sum_avglife") ,box(title = "Distribution de l'espérance de vie", solidHeader = T, status = "primary",plotOutput("plotLife"), width = 9)),
                tabPanel("Average Wellbeing","Variable Quantitative",verbatimTextOutput("sum_avgwlg") ,box(title = "Distribution du bien être", solidHeader = T, status = "primary",plotOutput("plotWell"), width = 9)),
                tabPanel("Happy Life Years","Variable Quantitative",verbatimTextOutput("sum_happy") ,box(title = "Distribution de l'espérance de vie heureuse", solidHeader = T, status = "primary",plotOutput("plotHappy"), width = 9)),
                tabPanel("Footprint","Variable Quantitative", verbatimTextOutput("sum_foot") ,box(title = "Distribution de l'empreinte écologique", solidHeader = T, status = "primary",plotOutput("plotFoot"), width = 9)), 
                tabPanel("Inequality of Outcomes","Variable Quantitative",verbatimTextOutput("sum_ineq") ,box(title = "Distribution des inégalités d'espérance de vie et de bien être", solidHeader = T, status = "primary",plotOutput("plotInegal"), width = 9)),
                tabPanel("Happy Planet Index","Variable Quantitative",verbatimTextOutput("sum_happyplanet") ,box(title = "Distribution de l'indice de bonheur", solidHeader = T, status = "primary",plotOutput("plotPlanet"), width = 9)),
                tabPanel("GDP/capita","Variable Quantitative", verbatimTextOutput("sum_gdp") ,box(title = "Distibution du PIB par habitant", solidHeader = T, status = "primary",plotOutput("plotPIB"), width = 9)),
                tabPanel("Population","Variable Quantitative", verbatimTextOutput("sum_pop") ,box(title = "Distribution du nombre d'habitants", solidHeader = T, status = "primary",plotOutput("plotPopulation"), width = 9))
              )),
              box(title = "Analyse", solidHeader = T, status = "primary",HTML("La majorité des pays est issue d’Afrique subsaharienne suivi des pays post-communistes, qui comprend les anciens pays communistes d’Europe de l’Est et d’Asie.
              <br>On a l’estimation de l’espérance de vie dans le monde. On constate que 50% de la population décède entre 65 ans et 77 ans. C'est à Hong Kong que l'on vit le plus longtemps : jusqu'à 83,6 ans en moyenne. Au contraire, c'est le Swaziland qui a l'espérance de vie la plus faible avec 48.9 ans.
              <br>Concerant le bien-être, les estimations sont comprises entre 3 et 8. C’est les populations qui se notent sur 10. Ceux qui se sentent le mieux sont les suisses (7.8) tandis que les togolais se donnent la note la plus basse (2.9).
              <br>Quant à l’espérance d’années de vie heureuses, le 1er quartile est à 18 ans et le 3eme quartile à 39 ans. Ceux qui vivent le moins de temps heureux sont les togolais (9 ans) alors que ceux heureux le plus longtemps sont les Suisses (59.3 ans).
              <br>Pour l’empreinte écologique par habitant, on remarque des valeurs extrèmes, elles concernent le Luxembourg à 11.7 hectares globaux par personne et l’Australie à 10.7. La plus faible est pour l’Haïti (0.6).
              <br>Concernant les inégalités d’espérance de vie et de bien-être au sein d'une même population, les plus élevées sont pour le Chad et les plus basses au Pays-Bas.
              <br>L’indice de bonheur le plus élevé est pour le Costa Rica,à 44.7, tandis que le plus faible est pour le Chad à 12.8.
              <br>Quant à la distribution du PIB par habitant, elle est très éparpillé, 50% des pays ont un PIB par habitant compris entre 1500$ et 15000$ tandis que les 2 valeurs maximales sont pour le Luxembourg et la Norvège à 105 000$ et 101 000$. Le plus faible est le Burundi à 244$."))
              ),
      tabItem(
        "continent", 
        tags$h1 ("Inégalités entre continents"),
        fluidPage(navlistPanel(
          tabPanel("Average Life ",box(title = "Espérance de vie en fonction des continents", solidHeader = T, status = "primary",plotOutput("plotCompareLife"), width = 12)),
          tabPanel("Average Wellbeing",box(title = "Bien être en fonction des continents",solidHeader = T, status = "primary",plotOutput("plotCompareWell"), width = 12)),
          tabPanel("Footprint",box(title = "Empreinte écologique en fonction des continents",solidHeader = T, status = "primary",plotOutput("plotCompareFoot"), width = 12)), 
          tabPanel("Happy Planet Index",box(title = "Indice de bonheur en fonction des continents",solidHeader = T, status = "primary",plotOutput("plotComparePlanet"), width = 12))
        ))),
      tabItem(
        "carte",style="overflow-y : scroll;overflow-x: scroll;", tags$h1("Carte mondiale du bonheur"),
        box(title = "Les pays avec les plus haut indices de bonheur", solidHeader=TRUE,status = "success",leafletOutput("map2"), width = 12),
        box(title = "Les pays avec les plus bas indices de bonheur",solidHeader=TRUE,status = "danger",leafletOutput("map"), width = 12)),
      tabItem(
        "graph", tags$h1("Evolution du bonheur"),
        box(title = "Croisement de 4 variables",solidHeader = T, status = "primary",plotOutput("causes"), width = 9),
        box(title = "Commentaires",solidHeader = T, status = "primary", width = 6,
        HTML("On compare ici le rang des pays sur l'indice de bonheur et leur empreinte écologique.
             <br>Cela nous permet de voir quel rapport les pays les mieux classés ont souvent une empreinte écologique élevée et inversement.
             <br>Il a été ajouté à cela un code couleur mis en place dans le but de distinguer les différents continents.
             <br>La grosseur des points permet de mesurer la taille de la population de chaque pays"))),
      tabItem("acp", tags$h1("Analyse en compasantes principales"),
              box(title="Graphique variables", solidHeader = T, status = "primary",plotOutput("ACP"), width = 9),
              box(title="Commentaires",solidHeader = T, status = "primary", width = 6,
                  HTML(" * Sur ces 2 axes on garde presque 80% de l'information, ce qui est plutôt un bon résultat.
                       <br>Les variables les mieux représentés sont celles qui sont proches du cercle : c'est le cas pour toutes des variables sauf le variable population qui est proche de l'origine, qui est donc mal réprésentée.
                       <br>* L'axe 1 oppose l'espérance de vie, l'espérance de vie heureuse et le bien être moyen à l'inégalité de répartition. Cela signifie que plus l'espérance de vie et le bien ètre est élevé pour un pays, moins les inégalités ont tendances à ètre grandes pour ces 2 variables.
                       <br>* L'axe 2 opposoe lui l'indice de bonheur et le nombre d'habitants au PIB par habitant et à l'empreinte écologique par habitant. Cela signifique que plus l'indice de bonheur est élevé, moins le PIB par habitant est élevé, ce qui peut etonner. Egalement, plus le nombre d'habitants est grand, plus l'empreinte écologique par habitant est grand.")
                  )),
      tabItem("clusters",style="overflow-y : scroll;overflow-x: scroll;", tags$h1("Clusters"),
              box(title = "Evalutation des solutions",solidHeader = T, status = "primary", collapsible = T,plotOutput("solutions"), width = 6
                  #,HTML("Ce graphique permet de voir que la meilleure solution est de choisir 2 clusters car c'est le point le plus haut sur le graphique")
                  ),
              box(title = "Dendogramme", solidHeader = T, status = "primary", collapsible = T,plotOutput("dendogramme"), width = 6),
              box(title = "Cractéristiques des clusters", solidHeader = T, status = "primary", collapsible = T,verbatimTextOutput("capture"), width = 6,
                  HTML("<br>La méthode kmeans nous a permis de classer les individus dans 2 groupes.
                       <br>Le 1er cluster regroupe 74 pays où les habitants ont une espérance de vie élevée, un bien-ètre élevée, une empreinte écologique importante, peu d'inégalités en terme de bien ètre et d'espérance de vie, un PIB par habitant élevé --> pays ayant un indice de bonheur important
                       <br>Le 2e cluster regroupe 66 pays où les habitants où une espérance de vie faible, un bien-ètre plutot faible, une empreinte écologique peu importante, des inégalités fortes en terme de bien ètre et d'espérance de vie, un PIB par habitant faible --> pays avec un indice de bonheur peu élevé.")),
              box(title ="Distribution des pays dans les classes",solidHeader = T, status = "primary", collapsible = T, dataTableOutput("tablee"), width = 6)
                  ))),
  title = "Projet",
  skin = "purple"
                  )

server <- function(input, output) {
  
  output$tablee <- renderDataTable({
    dataCluster = read_xlsx("hpi-data-2016.xlsx", sheet = "Feuil1")
  })
  
  output$gg_americas <- renderPrint({
    ggplot(data, aes(x=data$`HPI Rank`, y=data$HappyPlanetIndex)) +
      geom_point(size=2, shape=23)
  })
  
  output$gg_asia <- renderPrint({
    ggplot(data, aes(x=data$`HPI Rank`, y=data$HappyPlanetIndex)) +
      geom_point(size=2, shape=23)
  })
  
  output$gg_europe <- renderPrint({
    ggplot(data, aes(x=data$`HPI Rank`, y=data$HappyPlanetIndex)) +
      geom_point(size=2, shape=23)
  })
  
  output$gg_eastafrica <- renderPrint({
    ggplot(data, aes(x=data$`HPI Rank`, y=data$HappyPlanetIndex)) +
      geom_point(size=2, shape=23)
  })
  
  output$gg_post <- renderPrint({
    ggplot(data, aes(x=data$`HPI Rank`, y=data$HappyPlanetIndex)) +
      geom_point(size=2, shape=23)
  })
  
  output$gg_subsah <- renderPrint({
    ggplot(data, aes(x=data$`HPI Rank`, y=data$HappyPlanetIndex)) +
      geom_point(size=2, shape=23)
  })
  
  output$sum_region <- renderPrint({
    table(data$Region)
  })
  
  output$sum_avglife <- renderPrint({
    summary(data$AverageLifeExpectancy)
  })
  
  output$sum_avgwlb <- renderPrint({
    summary(data$`AverageWellbeing
            `)
  })
  
  output$sum_happy <- renderPrint({
    summary(data$HappyLifeYears)
  })
  
  output$sum_foot <- renderPrint({
    summary(data$`Footprint(gha/capita)`)
  })
  
  output$sum_ineq <- renderPrint({
    summary(data$InequalityofOutcomes)
  })
  
  output$sum_happyplanet<- renderPrint({
    summary(data$HappyPlanetIndex)
  })
  
  output$sum_gdp<- renderPrint({
    summary(data$`GDP/capita
            ($PPP)`)
  })
  
  output$sum_pop<- renderPrint({
    summary(data$Population)
  })
  
  output$capture <- renderPrint({
    groupes.kmeans <- kmeans(data.cr,centers=2,nstart=5)
    print(groupes.kmeans)
    print(table(groupes.cah,groupes.kmeans$cluster))
  })
  
  output$plotRegion <- renderPlot({
    plotRegion = barplot(plotRegion, main = "Répartition géographique des pays de l'étude", xlab = "Région du globe", ylab = "Nombre de pays")
  })
  output$plotLife <- renderPlot({
    plotLife = boxplot(data$AverageLifeExpectancy, main = "Distribution de l'espérance de vie", ylab = "Années")
  })
  output$plotWell <- renderPlot({
    plotWell = boxplot(data$AverageWellbeing, main = "Distribution du bien ètre", ylab = "Note sur 10")
  })
  output$plotHappy <- renderPlot({
    plotHappy = boxplot(data$HappyLifeYears, main = "Distribution de l'espérance de vie heureuse", ylab = "Années")
  })
  output$plotFoot <- renderPlot({
    plotFoot = boxplot(data$Footprint, main = "Distribution de l'empreinte écologique", ylab = "hectar global par habitant")
  })
  output$plotInegal <- renderPlot({
    plotInegal = boxplot(data$InequalityofOutcomes, main = "Distribution des inégalités en espérance de vie et bien ètre", ylab = "% d'inégalités")
  })
  
  output$plotPlanet <- renderPlot({
    plotPlanet = boxplot(data$HappyPlanetIndex, main = "Distribution de l'indice de bonheur", ylab = "Indice de bonheur")
  })
  output$plotPIB <- renderPlot({
    plotPIB = boxplot(data$GDPpercapita, main = "Distribution du PIB par habitant", ylab = "PIB par habitant")
  })
  output$plotPopulation <- renderPlot({
    plotPopulation = boxplot(data$Population, main = "Distribution des populations", ylab = "Nombre d'habitants")
  })
  output$plotCompareLife <- renderPlot({
    plotCompareLife = boxplot(dataPlot$AverageLifeExpectancy~dataPlot$RegionPlot,names=c("Americas","Asia Pacific", "Europe", "Middle East and North Africa", "Post-communist", "Sub Saharan Africa"),xlab = "Région", ylab="Espérance de vie")
  })
  output$plotCompareWell <- renderPlot({
    plotCompareWell = boxplot(dataPlot$AverageWellbeing~dataPlot$RegionPlot,names=c("Americas","Asia Pacific", "Europe", "Middle East and North Africa", "Post-communist", "Sub Saharan Africa"), xlab = "Région", ylab = "Bien-être (note sur 10)")
  })
  output$plotCompareFoot <- renderPlot({
    plotCompareFoot = boxplot(dataPlot$Footprint~dataPlot$RegionPlot,names=c("Americas","Asia Pacific", "Europe", "Middle East and North Africa", "Post-communist", "Sub Saharan Africa"), xlab = "Région", ylab = "Empreinte écologique")
  })
  output$plotComparePlanet <- renderPlot({
    plotComparePlanet = boxplot(dataPlot$HappyPlanetIndex~dataPlot$RegionPlot,names=c("Americas","Asia Pacific", "Europe", "Middle East and North Africa", "Post-communist", "Sub Saharan Africa"), xlab = "Région", ylab = "Indice de bonheur")
  })
  
  # image1 creates a new PNG file each time Radius changes
  #output$capture <- renderImage({
    # Get width and height of image1
   # width  <- 919
    #height <- 160
    
    # A temp file to save the output.
    # This file will be automatically removed later by
    # renderImage, because of the deleteFile=TRUE argument.
    #outfile <- tempfile(fileext = "Capture.PNG")
    
    # Generate the image and write it to file
    #x <- matrix(rep((0:(width-1))/(width-1), height), height,
     #           byrow = TRUE)
    #y <- matrix(rep((0:(height-1))/(height-1), width), height)
    #gauss2d <- function(x, y, r = 0.15) {
     # exp(
      #  -((x - 0.5)^2 + (y - 0.5)^2) /
       #   (2 * r^2)
    #  )
    #}
    #pic <- gauss2d(x, y)
    #writePNG(pic, target = outfile)
    
    # Return a list containing information about the image
    #list(src = "Capture.PNG",
     #    contentType = "image/png",
      #   width = width,
       #  height = height,
        # alt = "capture")
    
  #}, deleteFile = F)
  
  output$ACP <- renderPlot({
    plotACP = plot(res, choix="var")
  })
  output$dendogramme <- renderPlot({
    plotCluser = plot(cah.ward)
  })
  output$solutions <- renderPlot({
    plotsolution = plot(1:10,sol.kmeans$crit,type="b",xlab="Nb. de groupes",ylab="Silhouette")
  })
  
  output$causes <- renderPlot({
    ggplot(data2, aes(x=data2$Footprint, y=data2$`HPI Rank`, fill = data2$Region, size = data2$Population)) +
      geom_point(shape=21) +
      labs(x = "Emprunte écologique nationale",
           y = "HPI Rank",
           title = "Emprunte écologique dans le monde") +
      theme_light()
  })

  output$map2 <- renderLeaflet({
    carte2
  })  
  output$map <- renderLeaflet({
    carte
  })
}

shinyApp(ui, server)

