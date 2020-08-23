library(shiny)
library(rsconnect)
library(DT)
library(igraph)
library(dplyr)
source("utils.R")

shinyServer(function(input, output, session) {
  # Network_Aに入力があるか確認
  file_network_A <- reactive({
    inFile <- input$network_A
    if (is.null(inFile)){ return(NULL) } #  エラー処理
    df1 <<- read.table(inFile$datapath, header = FALSE)
    return(df1)
  })

  # Network_Aのエッジリストを出力
  output$data1 <- DT::renderDataTable({
    if (is.null(file_network_A())) { return(NULL) } #  エラー処理
    datatable(df1, options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 20, 100)))
    })

  # Network_Aの内容をグラフで表現
  output$plot1 <- renderPlot({
    if (is.null(file_network_A())) { return(NULL) } #  エラー処理
    par(ps = 20)
    plot(graph.data.frame(df1, directed=F),
    frame = TRUE,
    layout = layout_nicely,
    vertex.label.font = 2,
    vertex.label.cex = 1.0,
    vertex.frame.color = "white",
    vertex.size = input$range,
    vertex.color = "lightblue",
    edge.width = 4,
    edge.color = "gray48")
  })

  # Network_Bに入力があるか確認
  file_network_B <- reactive({
    inFile <- input$network_B
    if (is.null(inFile)){ return(NULL) } #  エラー処理
    df2 <<- read.table(inFile$datapath, header = FALSE)
    return(df2)
  })

  # Network_Bのエッジリストを出力
  output$data2 <- DT::renderDataTable({
    if (is.null(file_network_B())) { return(NULL) } #  エラー処理
    datatable(file_network_B(), options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 20, 100)))
    })

  # Network_Bの内容をグラフで表現
  output$plot2 <- renderPlot({
    if (is.null(file_network_B())) { return(NULL) } #  エラー処理
    par(ps = 20)
    plot(graph.data.frame(df2, directed=F),
    frame = TRUE,
    layout = layout_nicely,
    vertex.label.font = 2,
    vertex.label.cex = 1.0,
    vertex.frame.color = "white",
    vertex.size = input$range,
    vertex.color = "lightblue",
    edge.width = 4,
    edge.color = "gray48")
  })

  # 差分ネットワーク作成用に入力データを再構成（utils.R内のdiffNetを参照）
  differential_network <- reactive({
    if(is.null(file_network_A()) || is.null(file_network_B())) { return(NULL) } #  エラー処理
    each <<- diffNet(df1, df2)
    return(each)
  })

  # Network_Aのみで表現されているエッジをグラフ化（Positive Differential Network）
  output$plot3 <- renderPlot({
    if(is.null(differential_network())) { return(NULL) } #  エラー処理
    par(ps = 20)
    plot(graph.data.frame(each$df1, directed=F),
    frame = TRUE,
    layout = switch(input$layout, "Grid type" = layout_on_grid, "Nicely type" = layout_nicely),
    vertex.label.font = 2,
    vertex.label.cex = 1.0,
    vertex.frame.color = "white",
    vertex.size = input$range,
    vertex.color = "lightblue",
    edge.width = 4,
    edge.color = "royalblue")
  })

  # Network_Bのみで表現されているエッジをグラフ化（Negative Differential Nertwork）
  output$plot4 <- renderPlot({
    if(is.null(differential_network())) { return(NULL) } #  エラー処理
    par(ps = 20)
    plot(graph.data.frame(each$df2, directed=F),
    frame = TRUE,
    layout = switch(input$layout, "Grid type" = layout_on_grid, "Nicely type" = layout_nicely),
    vertex.label.font = 2,
    vertex.label.cex = 1.0,
    vertex.frame.color = "white",
    vertex.size = input$range,
    vertex.color = "lightblue",
    edge.width = 4,
    edge.color = "orangered3")
  })

  # 上記のPositive/Negativeネットワークを一つのグラフで表現
  output$plot5 <- renderPlot({
    if(is.null(file_network_A()) || is.null(file_network_B())) { return(NULL) } #  エラー処理
    ecol <- ifelse(each$df3$weight > 0, "royalblue", "orangered3")
    par(ps = 20)
    plot(graph.data.frame(each$df3, directed=F),
    frame = TRUE,
    layout = switch(input$layout, "Grid type" = layout_on_grid, "Nicely type" = layout_nicely),
    vertex.label.font = 2,
    vertex.label.cex = 1.0,
    vertex.frame.color = "white",
    vertex.size = input$range,
    vertex.color = "lightblue",
    edge.width = 4,
    edge.color = ecol)
  })
  #  Differential Networkのエッジリストを出力
  output$data3 <- DT::renderDataTable({
    if (is.null(differential_network())) { return(NULL) } #  エラー処理
    datatable(each$df3, options = list(
      pageLength = 5,
      lengthMenu = c(5, 10, 20, 100)))
  })

  # 差分ネットワークの評価に使われる二つの指標を計算（utils.R内のnetcesを参照）
  calculate_parameter <- reactive({
    if(is.null(file_network_A()) || is.null(file_network_B())) { return(NULL) } #  エラー処理
    parameter <<- netces(df1, df2)
    return(parameter)
  })

  # 上記で計算される指標を出力
  output$data4 <- DT::renderDataTable({
    if (is.null(calculate_parameter())) { return(NULL) } #  エラー処理
    datatable(as.data.frame(parameter), options = list(
      pageLength = 10,
      lengthMenu = c(5, 10, 20, 100)))
  })
})
