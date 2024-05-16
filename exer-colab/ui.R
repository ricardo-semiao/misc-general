style <- 
  "
    .tabbable .nav.nav-pills.shiny-tab-input.shiny-bound-input {
            border-top: 1px solid #800000;
            border-bottom: 1px solid #800000;
            margin: 10px 0;
    }
    .nav-pills>li.active>a, .nav-pills>li.active>a:focus, .nav-pills>li.active>a:hover {
      background-color: #800000; /* Red background for the active tab */
      color: #fff; /* White text color for the active tab */
    }
    .nav.nav-pills.shiny-tab-input.shiny-bound-input [role='tab'][aria-selected='false'] {
      color: #800000; /* Text color for non-active tabs */
    }
    .red-rule {
      border-top: 2px solid #800000;
      margin-top: 5px;
      margin-bottom: 5px;
    }
    .red-rule-small {
      border-top: 1px solid #800000;
      margin-top: 5px; 
      margin-bottom: 5px;
    }
    .half-rule {
      border-top: 1px solid #800000;
      width: 90%;
      margin: 20px auto;
    }
    h2 {
      font-weight: bold;
      text-align: center;
    }
    h3, h4, h5 {
      font-weight: bold;
    }
    a {
      color: #800000; /* Red color for actionLink */
    }
  "

script <-
  '
  $(document).ready(function() {
    var toggleStatus = false;

    $("#toggleAllButton").click(function() {
      if (toggleStatus) {
        $(".togglex").click();
        toggleStatus = false;
        $("#toggleAllButton").text("Toggle All");
      } else {
        $(".togglex").click();
        toggleStatus = true;
        $("#toggleAllButton").text("Untoggle All");
      }
    });
  });
  '

home_ui <-
  fluidRow(
    column(width = 12,
           h2("Bem vindo!"),
           p("Esse é o app para os exercícios do curso 'R de Ricardo'."),
           p(tags$b("Como usar:")),
           tags$ol(
             tags$li("Na barra de navegação, clique na lista quista;"),
             tags$li("Escolha uma questão para responder, e faça o upload de texto, código, e/ou uma imagem com sua resposta;"),
             tags$li("Clique no botão 'Postar resposta' para sobrescrever a resposta anterior. Use o botão 'Voltar' para voltar a anterior [em construção];"),
             tags$li("No final de cada lista, há uma seção 'Recapitulando' listando as funções/temas tratados na lista;"),
             tags$li("Para evitar mudanças conflitantes, é permitido apenas um usuário por vez. Se receber o erro 'Service unavailable' (503), volte em outro momento.")
           ),
           p("Esse aplicativo está em sua versão beta. Qualquer dúvida ou bug, favor reportar para o autor."),
           p(tags$b("Autor: "), "Ricardo Semião e Castro (ricardo.semiao@outlook.com).")
    )
  )

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(
      HTML(style),
      tags$title("Curso R - Exercícios colaborativos")
    )
    #, tags$script(HTML(script))
  ),
  h1("Curso R - Exercícios colaborativos"),
  mainPanel(
    tabsetPanel(
      id = "tabs",
      type = "pills",
      tabPanel("Home", home_ui),
      tabPanel("Lista 1", buildset_ui("set1"), value = "set1"),
      tabPanel("Lista 2", buildset_ui("set2"), value = "set2")
      #, tabPanel("Teste", uiOutput("test"))
    )
  )
)