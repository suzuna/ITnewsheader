library(shiny)


shinyUI(
    fluidPage(
        tags$head(tags$style(HTML("body {font-size: 16px; max-width: 980px; margin: 0 auto;} a {color: black;}"))),
        titlePanel("ITnewsheader"),
        hr(),
        mainPanel(
            tabsetPanel(
                type="tabs",
                tabPanel("ITmedia",br(),DTOutput("ITmedia")),
                tabPanel("ITmedia_news",br(),DTOutput("ITmedia_news")),
                tabPanel("ITmedia_pcuser",br(),DTOutput("ITmedia_pcuser")),
                tabPanel("ITmedia_enterprise",br(),DTOutput("ITmedia_enterprise")),
                tabPanel("ITmedia_mobile",br(),DTOutput("ITmedia_mobile")),
                tabPanel("ITmedia_business",br(),DTOutput("ITmedia_business")),
                tabPanel("atmarkit",br(),DTOutput("atmarkit")),
                tabPanel("netlabo",br(),DTOutput("netlabo")),
                tabPanel("nikkei_xtech",br(),DTOutput("nikkei_xtech")),
                tabPanel("ascii",br(),DTOutput("ascii")),
                tabPanel("ascii_ai",br(),DTOutput("ascii_ai")),
                tabPanel("ascii_biz",br(),DTOutput("ascii_biz")),
                tabPanel("ascii_tech",br(),DTOutput("ascii_tech")),
                tabPanel("ascii_akiba",br(),DTOutput("ascii_akiba")),
                tabPanel("impress",br(),DTOutput("impress")),
                tabPanel("impress_internet",br(),DTOutput("impress_internet")),
                tabPanel("impress_pc",br(),DTOutput("impress_pc")),
                tabPanel("impress_akiba",br(),DTOutput("impress_akiba")),
                tabPanel("impress_ktai",br(),DTOutput("impress_ktai")),
                tabPanel("impress_cloud",br(),DTOutput("impress_cloud")),
                tabPanel("impress_hobby",br(),DTOutput("impress_hobby"))
            ),
            width=12
        )
    )
)
