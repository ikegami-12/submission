
shinyUI(fluidPage(
  titlePanel("differential Network"),
  sidebarLayout(

    sidebarPanel(
      fileInput("network_A", label = h4("Input network A (written as an edgelist)")),
      fileInput("network_B", label = h4("Input network B (written as an edgelist)")),
      sliderInput("range", label = h4("Size of node"), min = 1, max = 30, value = 20),
      selectInput("layout", label = h4("Layout type (only differential network)"),
      choices = list("Grid type", "Nicely type"),
      selected = "Grid type"
    ),
      width = 3,
    ),

    mainPanel(
      tabsetPanel(type = "tabs",
        tabPanel("Instructions",
        h4("アプリケーションの使い方", style = "font-size: 20pt"),
        p("エッジリストからネットワーク（グラフ）を作成するアプリケーションです。", style = "font-size: 13pt; line-height:0.9;"),
        p("左のサイドバーに入力を与えることで、自動的にグラフを作成します。", style = "font-size: 13pt; line-height:0.9;"),
        p("加えて差分ネットワークを出力し、NESHスコアとdBを計算します。", style = "font-size: 13pt; line-height:0.9;"),
        p("サイドバー下部のSize of nodeでノードの大きさを調整できます。", style = "font-size: 13pt; line-height:0.9;"),
        p("また、Layout typeでグラフの形を変更できます（差分ネットワークにのみ適用）", style = "font-size: 13pt; line-height:0.9;"),
        br(),
        p(span("Edgelist", style = "font-weight: bold;")," : グラフにおけるノード（○）間の関係性を複数行２列で表したもの。同行のV1とV2が関係を持つ。", style = "font-size: 13pt;"),
        p(span("Network", style = "font-weight: bold;")," : 物と物の関係を図で表したもの。ノード（○）間で関係を持つとき、エッジ（ー）が引かれる。",style = "font-size: 13pt;"),
        p(span("Differential Network", style = "font-weight: bold;")," : 二つのネットワークの違いを一つのネットワークで表したもの。PositiveはネットワークAのみで表現されているエッジリストであり、NegativeはネットワークBのみで表現されているエッジリストをグラフ化したものである。下部に表示されるのDifferential Networkはそれらを一つのグラフで表現したものである。",style = "font-size: 13pt;"),
        p(span("Parameter", style = "font-weight: bold;")," : Differential Networkを評価する際の２つのパラメータを記載。NESH値は各ノードにおける隣接ノードの変化を表し、dBは各ノードにおける媒介中心性の変化量を表す。",style = "font-size: 13pt;"),
        ),

        tabPanel("Edgelist",
          fluidRow(
            h3("Edgelist of Network A"),
            DT::dataTableOutput("data1"),
            br()
          ),
          fluidRow(
            h3("Edgelist of Network B"),
            DT::dataTableOutput("data2"),
            br()
          ),
          fluidRow(
            h3("Edgelist of Differential Network"),
            DT::dataTableOutput("data3")),
            br(),br()
          ),

        tabPanel("Network",
          fluidRow(
            column(6,  h3("Graph of Network A")),
            column(6,  h3("Graph of Network B"))
          ),
          fluidRow(
            column(6, plotOutput("plot1", width = "100%", height = "400px")),
            column(6, plotOutput("plot2", width = "100%", height = "400px"))
          )
        ),

        tabPanel("Differential Network",
          fluidRow(
            column(6,  h3("Positive Differential Network")),
            column(6,  h3("Negative Differential Network"))
          ),
          fluidRow(
            column(6, plotOutput("plot3", width = "100%", height = "400px")),
            column(6, plotOutput("plot4", width = "100%", height = "400px"))
          ),
          fluidRow(
            h3("Differential Network"),
            plotOutput("plot5", width = "100%", height = "500px"))
          ),

        tabPanel("Parameter",
          h3("NESH and dB in each node"),
          DT::dataTableOutput("data4")
        )
      )
    )
  )
))
