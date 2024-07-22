server <- function(input, output, session) {
  #############################################################################
  
  # dashboard
  
  # fill the box
  
  output$total_scorecard_ds <- renderValueBox({
    total_scorecard_ds <- scorecard_all %>%
      summarise(Jumlah = length(recommendation))
    
    valueBox(
      paste0(total_scorecard_ds$Jumlah),
      "Total Data",
      color = "aqua",
      icon = icon("list")
    )
    
  })
  
  output$good_ds <- renderValueBox({
    good_ds <- scorecard_all %>%
      group_by(recommendation) %>%
      filter(recommendation == "GOOD") %>%
      summarise(Jumlah = length(recommendation))
    
    valueBox(paste0(good_ds$Jumlah),
             "GOOD",
             color = "olive",
             icon = icon("list"))
    
  })
  
  output$bad_ds <- renderValueBox({
    bad_ds <- scorecard_all %>%
      group_by(recommendation) %>%
      filter(recommendation == "BAD") %>%
      summarise(Jumlah = length(recommendation))
    
    valueBox(
      paste0(bad_ds$Jumlah),
      "BAD",
      color = "maroon",
      icon = icon("list")
    )
  })
  
  # plot_scorecard_recom
  
  output$plot_scorecard_recom <- renderPlotly({
    scorecard_recom <- scorecard_all %>%
      group_by(recommendation) %>%
      summarise(Jumlah = length(recommendation))
    
    plot_scorecard_recom <- ggplot(scorecard_recom, aes(x = recommendation, y = Jumlah)) +
      geom_col(aes(fill = Jumlah),
               width = 0.5,
               show.legend = F) +
      coord_flip() +
      theme_algoritma
    
    ggplotly(plot_scorecard_recom, height = 190)
    
  })
  
  # df scorecard dashboard
  
  output$df_scorecard_ds <- renderDataTable({
    datatable(
      scorecard_all,
      extensions = 'Buttons',
      filter = 'top',
      options = list(
        dom = "Blfrtip",
        buttons = list(
          "copy",
          list(
            extend = "collection",
            buttons = c("csv", "excel", "pdf"),
            text = "Download"
          )
        ),
        # end of buttons customization
        # customize the length menu
        lengthMenu = list(c(10, 100, 1000, -1), # declare values
                          c(10, 100, 1000, "All")),
        # end of lengthMenu customization
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "250px"
      ) # end of options
    )
    
  })
  
  # plot_scorecard_sex
  
  output$plot_scorecard_sex <- renderPlotly({
    agg_scorecard_sex <- scorecard_all %>%
      group_by(recommendation, sex) %>%
      summarise(Jumlah = length(sex))
    agg_scorecard_sex
    
    plot_scorecard_sex <- ggplot(agg_scorecard_sex, aes(x = sex, y = Jumlah)) +
      geom_col(aes(fill = recommendation), position = "dodge") +
      theme_algoritma +
      labs(x = "", y = "Jumlah")
    
    ggplotly(plot_scorecard_sex)
    
  })
  
  # plot_scorecard_marriage
  
  output$plot_scorecard_marriage <- renderPlotly({
    agg_scorecard_marriage <- scorecard_all %>%
      group_by(recommendation, marriage) %>%
      summarise(Jumlah = length(marriage))
    agg_scorecard_marriage
    
    plot_scorecard_marriage <- ggplot(agg_scorecard_marriage, aes(x = marriage, y = Jumlah)) +
      geom_col(aes(fill = recommendation), position = "dodge") +
      theme_algoritma +
      labs(x = "", y = "Jumlah")
    
    ggplotly(plot_scorecard_marriage)
    
  })
  
  # plot_scorecard_education
  
  output$plot_scorecard_education <- renderPlotly({
    agg_scorecard_education <- scorecard_all %>%
      group_by(recommendation, education) %>%
      summarise(Jumlah = length(education))
    agg_scorecard_education
    
    plot_scorecard_education <- ggplot(agg_scorecard_education, aes(x = education, y = Jumlah)) +
      geom_col(aes(fill = recommendation), position = "dodge") +
      theme_algoritma +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    labs(x = "", y = "Jumlah")
    
    ggplotly(plot_scorecard_education)
    
  })
  
  # plot_scorecard_hist
  
  output$plot_scorecard_hist <- renderPlotly({
    scorecard_all %>%
      ggplot(aes(x = limit_bal)) +
      geom_histogram(
        bins = input$bins_1,
        fill = "#69b3a2",
        color = "#e9ecef",
        alpha = 0.9
      ) +
      labs(title = "Distribution of Limit Balance") +
      theme(plot.title = element_text(size = 15))
    
  })
  
  # plot_scorecard_age
  
  output$plot_scorecard_age <- renderPlotly({
    agg_scorecard_age <- scorecard_all %>%
      group_by(recommendation, age) %>%
      summarise(Jumlah = length(age))
    agg_scorecard_age
    
    plot_scorecard_age <- ggplot(data = agg_scorecard_age,
                                 mapping = aes(x = recommendation, y = age)) +
      geom_boxplot(
        outlier.shape = NA,
        aes(fill = recommendation),
        alpha = 0.7,
        show.legend = FALSE
      ) +
      geom_jitter(
        mapping = aes(size = Jumlah),
        col = "black",
        alpha = 0.2
      ) +
      labs(
        title = "",
        subtitle = "",
        caption = "",
        y = "Age",
        x = "",
        size = ""
      ) +
      scale_fill_brewer(palette = "Set1")
    
    ggplotly(plot_scorecard_age)
    
  })
  #############################################################################
  
  # import data scorecard
  
  output$df_scorecard_1 <- renderDataTable({
    file <- input$file1
    ext <- tools::file_ext(file$datapath)
    
    req(file)
    validate(need(ext == "xlsx", "Please upload a xlsx file"))
    
    read_xlsx(file$datapath)
    
    datatable(
      read_xlsx(file$datapath),
      extensions = 'Buttons',
      filter = 'top',
      options = list(
        dom = "Blfrtip",
        buttons = list(
          "copy",
          list(
            extend = "collection",
            buttons = c("csv", "excel", "pdf"),
            text = "Download"
          )
        ),
        # end of buttons customization
        # customize the length menu
        lengthMenu = list(c(10, 100, 1000, -1), # declare values
                          c(10, 100, 1000, "All")),
        # end of lengthMenu customization
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "250px"
      ) # end of options
    )
    
  })
  
  #  input cut_off
  
  observeEvent(input$input_cutoff_1, {
    input_scorecard_1 <-
      c(input$nilai_cutoff_1)
    
    if (any(input_scorecard_1 <= 0)) {
      showModal(modalDialog("Isi Nilai Cut-Off !!!"))
      
    }
    else {
      scorecard_1 <-
        data.frame(Waktu_Input = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      
      scorecard_2 <-
        data.frame(data_clean %>%
                     mutate(Cut_Off = as.numeric(input$nilai_cutoff_1)))
      
      # predict behaviour
      
      result_1 <- predict_behaviour(
        data = scorecard_2,
        score_card = score_card,
        cutoff = input$nilai_cutoff_1
      )
      result_1
      
      # output score & recommendation
      
      df_scorecard_1 <- cbind(scorecard_1, scorecard_2, result_1) %>%
        select(
          Waktu_Input,
          sex,
          marriage,
          limit_bal,
          education,
          age,
          pay_1,
          pay_2,
          pay_3,
          pay_4,
          pay_5,
          pay_6,
          bill_amt1,
          bill_amt2,
          bill_amt3,
          bill_amt4,
          bill_amt5,
          bill_amt6,
          pay_amt1,
          pay_amt2,
          pay_amt3,
          pay_amt4,
          pay_amt5,
          pay_amt6,
          Cut_Off,
          score,
          recommendation
        )
      
      output$total_scorecard_1 <- renderValueBox({
        total_scorecard_1 <- df_scorecard_1 %>%
          summarise(Jumlah = length(recommendation))
        
        valueBox(
          paste0(total_scorecard_1$Jumlah),
          "Total Data",
          color = "aqua",
          icon = icon("list")
        )
        
      })
      
      output$good_1 <- renderValueBox({
        good_1 <- df_scorecard_1 %>%
          group_by(recommendation) %>%
          summarise(Jumlah = length(recommendation)) %>%
          filter(recommendation == "GOOD")
        
        valueBox(paste0(good_1$Jumlah),
                 "GOOD",
                 color = "olive",
                 icon = icon("list"))
        
      })
      
      output$bad_1 <- renderValueBox({
        bad_1 <- df_scorecard_1 %>%
          group_by(recommendation) %>%
          summarise(Jumlah = length(recommendation)) %>%
          filter(recommendation == "BAD")
        
        valueBox(
          paste0(bad_1$Jumlah),
          "BAD",
          color = "maroon",
          icon = icon("list")
        )
        
      })
      
      # tampilkan data di datatable
      
      output$df_scorecard_1 <- renderDataTable({
        datatable(
          df_scorecard_1 <- cbind(scorecard_1, scorecard_2, result_1) %>%
            select(
              Waktu_Input,
              sex,
              marriage,
              limit_bal,
              education,
              age,
              pay_1,
              pay_2,
              pay_3,
              pay_4,
              pay_5,
              pay_6,
              bill_amt1,
              bill_amt2,
              bill_amt3,
              bill_amt4,
              bill_amt5,
              bill_amt6,
              pay_amt1,
              pay_amt2,
              pay_amt3,
              pay_amt4,
              pay_amt5,
              pay_amt6,
              Cut_Off,
              score,
              recommendation
            ),
          extensions = 'Buttons',
          options = list(
            dom = "Blfrtip",
            filter = 'top',
            buttons = list(
              "copy",
              list(
                extend = "collection",
                buttons = c("csv", "excel", "pdf"),
                text = "Download"
              )
            ),
            # end of buttons customization
            # customize the length menu
            lengthMenu = list(c(10, 100, 1000, -1), # declare values
                              c(10, 100, 1000, "All")),
            # end of lengthMenu customization
            pageLength = 10,
            scrollX = TRUE,
            scrollY = "250px"
          ) # end of options
        )
      })
      
    }
    
  })
  
  # save to excel
  
  observeEvent(input$save1, {
    if (input$nilai_cutoff_1 <= 0) {
      showModal(modalDialog("Nilai Cut Off Tidak Boleh Kosong !!"))
      
    } else {
      scorecard_1 <-
        data.frame(Waktu_Input = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      
      scorecard_2 <-
        data.frame(data_clean %>%
                     mutate(Cut_Off = as.numeric(input$nilai_cutoff_1)))
      
      # predict behaviour
      
      result_1 <- predict_behaviour(
        data = scorecard_2,
        score_card = score_card,
        cutoff = input$nilai_cutoff_1
      )
      result_1
      
      df_scorecard_1 <- cbind(scorecard_1, scorecard_2, result_1) %>%
        select(
          Waktu_Input,
          sex,
          marriage,
          limit_bal,
          education,
          age,
          pay_1,
          pay_2,
          pay_3,
          pay_4,
          pay_5,
          pay_6,
          bill_amt1,
          bill_amt2,
          bill_amt3,
          bill_amt4,
          bill_amt5,
          bill_amt6,
          pay_amt1,
          pay_amt2,
          pay_amt3,
          pay_amt4,
          pay_amt5,
          pay_amt6,
          Cut_Off,
          score,
          recommendation
        )
      
      # write .xlsx
      
      write_xlsx(df_scorecard_1, "scorecard_n.xlsx")
      
    }
    
  })
  
  # reload data scorecard from data input
  
  observeEvent(input$reload1, {
    reload_scorecard_1 <- read_xlsx("scorecard_n.xlsx")
    
    output$df_scorecard_1 <- renderDataTable({
      datatable(
        reload_scorecard_1,
        extensions = 'Buttons',
        filter = 'top',
        options = list(
          dom = "Blfrtip",
          buttons = list(
            "copy",
            list(
              extend = "collection",
              buttons = c("csv", "excel", "pdf"),
              text = "Download"
            )
          ),
          # end of buttons customization
          # customize the length menu
          lengthMenu = list(c(10, 100, 1000, -1), # declare values
                            c(10, 100, 1000, "All")),
          # end of lengthMenu customization
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "250px"
        ) # end of options
      )
      
    })
    
    output$total_scorecard_1 <- renderValueBox({
      total_scorecard_1 <- reload_scorecard_1 %>%
        summarise(Jumlah = length(recommendation))
      
      valueBox(
        paste0(total_scorecard_1$Jumlah),
        "Total Data",
        color = "aqua",
        icon = icon("list")
      )
      
    })
    
    output$good_1 <- renderValueBox({
      good_1 <- reload_scorecard_1 %>%
        group_by(recommendation) %>%
        summarise(Jumlah = length(recommendation)) %>%
        filter(recommendation == "GOOD")
      
      valueBox(paste0(good_1$Jumlah),
               "GOOD",
               color = "olive",
               icon = icon("list"))
      
    })
    
    output$bad_1 <- renderValueBox({
      bad_1 <- reload_scorecard_1 %>%
        group_by(recommendation) %>%
        summarise(Jumlah = length(recommendation)) %>%
        filter(recommendation == "BAD")
      
      valueBox(
        paste0(bad_1$Jumlah),
        "BAD",
        color = "maroon",
        icon = icon("list")
      )
      
    })
    
  })
  
  # clear data
  
  observeEvent(input$clear1, {
    output$df_scorecard_1 <- NULL
    output$total_scorecard_1 <- NULL
    output$good_1 <- NULL
    output$bad_1 <- NULL
    
  })
  
  ##############################################################################
  
  #  predict
  
  #  input data personal
  
  observeEvent(input$predict_cutoff, {
    input_scorecard_2 <-
      c(
        input$limit_bal,
        input$sex,
        input$education,
        input$marriage,
        input$age,
        input$pay_1,
        input$pay_2,
        input$pay_3,
        input$pay_4,
        input$pay_5,
        input$pay_6,
        input$bill_amt_1,
        input$bill_amt_2,
        input$bill_amt_3,
        input$bill_amt_4,
        input$bill_amt_5,
        input$bill_amt_6,
        input$pay_amt_1,
        input$pay_amt_2,
        input$pay_amt_3,
        input$pay_amt_4,
        input$pay_amt_5,
        input$pay_amt_6,
        input$cut_off
      )
    
    if (any(input_scorecard_2 < 0)) {
      showModal(modalDialog("Isikan Data dengan Benar & Lengkap !!"))
      
    }
    else {
      scorecard1 <-
        data.frame(
          Waktu_Input = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          sex = factor(input$sex),
          marriage = factor(input$marriage)
        )
      
      scorecard2 <-
        data.frame(
          limit_bal = as.numeric(input$limit_bal),
          education = factor(input$education),
          age = as.numeric(input$age),
          pay_1 = as.numeric(input$pay_1),
          pay_2 = as.numeric(input$pay_2),
          pay_3 = as.numeric(input$pay_3),
          pay_4 = as.numeric(input$pay_4),
          pay_5 = as.numeric(input$pay_5),
          pay_6 = as.numeric(input$pay_6),
          bill_amt1 = as.numeric(input$bill_amt_1),
          bill_amt2 = as.numeric(input$bill_amt_2),
          bill_amt3 = as.numeric(input$bill_amt_3),
          bill_amt4 = as.numeric(input$bill_amt_4),
          bill_amt5 = as.numeric(input$bill_amt_5),
          bill_amt6 = as.numeric(input$bill_amt_6),
          pay_amt1 = as.numeric(input$pay_amt_1),
          pay_amt2 = as.numeric(input$pay_amt_2),
          pay_amt3 = as.numeric(input$pay_amt_3),
          pay_amt4 = as.numeric(input$pay_amt_4),
          pay_amt5 = as.numeric(input$pay_amt_5),
          pay_amt6 = as.numeric(input$pay_amt_6),
          Cut_Off = as.numeric(input$cut_off)
        )
      
      # predict behaviour
      
      result2 <- predict_behaviour(
        data = scorecard2,
        score_card = score_card,
        cutoff = input$cut_off
      )
      result2
      
      output$df_scorecard_2 <- renderDataTable({
        datatable(
          cbind(scorecard1, scorecard2, result2),
          extensions = 'Buttons',
          filter = 'top',
          options = list(
            dom = "Blfrtip",
            buttons = list(
              "copy",
              list(
                extend = "collection",
                buttons = c("csv", "excel", "pdf"),
                text = "Download"
              )
            ),
            # end of buttons customization
            # customize the length menu
            lengthMenu = list(c(10, 100, 1000, -1), # declare values
                              c(10, 100, 1000, "All")),
            # end of lengthMenu customization
            pageLength = 10,
            scrollX = TRUE,
            scrollY = "250px"
          ) # end of options
        )
        
      })
      
      # output score & recommendation
      
      output$score <- renderValueBox({
        valueBox(
          paste0(result2$score),
          "Score",
          color = "maroon",
          icon = icon("search")
        )
        
      })
      
      output$recommendation <- renderValueBox({
        valueBox(
          paste0(result2$recommendation),
          "recommendation",
          color = "olive",
          icon = icon("list")
        )
        
      })
      
    }
    
  })
  
  # save data scorecard
  
  observeEvent(input$save2, {
    if (input$nilai_cutoff_1 < 0) {
      showModal(modalDialog("Nilai Cut Off Tidak Boleh Kosong !!"))
      
    } else {
      scorecard1 <-
        data.frame(
          Waktu_Input = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          sex = factor(input$sex),
          marriage = factor(input$marriage)
        )
      
      scorecard2 <-
        data.frame(
          limit_bal = as.numeric(input$limit_bal),
          education = factor(input$education),
          age = as.numeric(input$age),
          pay_1 = as.numeric(input$pay_1),
          pay_2 = as.numeric(input$pay_2),
          pay_3 = as.numeric(input$pay_3),
          pay_4 = as.numeric(input$pay_4),
          pay_5 = as.numeric(input$pay_5),
          pay_6 = as.numeric(input$pay_6),
          bill_amt1 = as.numeric(input$bill_amt_1),
          bill_amt2 = as.numeric(input$bill_amt_2),
          bill_amt3 = as.numeric(input$bill_amt_3),
          bill_amt4 = as.numeric(input$bill_amt_4),
          bill_amt5 = as.numeric(input$bill_amt_5),
          bill_amt6 = as.numeric(input$bill_amt_6),
          pay_amt1 = as.numeric(input$pay_amt_1),
          pay_amt2 = as.numeric(input$pay_amt_2),
          pay_amt3 = as.numeric(input$pay_amt_3),
          pay_amt4 = as.numeric(input$pay_amt_4),
          pay_amt5 = as.numeric(input$pay_amt_5),
          pay_amt6 = as.numeric(input$pay_amt_6),
          Cut_Off = as.numeric(input$cut_off)
        )
      
      result2 <- predict_behaviour(
        data = scorecard2,
        score_card = score_card,
        cutoff = input$cut_off
      )
      result2
      
      df_scorecard_2 <- cbind(scorecard1, scorecard2, result2)
      
      # write .xlsx
      
      write_xlsx(df_scorecard_2, "scorecard.xlsx")

    }
    
  })
  
  # reload data scorecard
  
  observeEvent(input$reload2, {
    reload_scorecard2 <- read_xlsx("scorecard.xlsx")
    
    output$df_scorecard_2 <- renderDataTable({
      datatable(
        reload_scorecard2,
        extensions = 'Buttons',
        filter = 'top',
        options = list(
          dom = "Blfrtip",
          buttons = list(
            "copy",
            list(
              extend = "collection",
              buttons = c("csv", "excel", "pdf"),
              text = "Download"
            )
          ),
          # end of buttons customization
          # customize the length menu
          lengthMenu = list(c(10, 100, 1000, -1), # declare values
                            c(10, 100, 1000, "All")),
          # end of lengthMenu customization
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "250px"
        ) # end of options
      )
      
    })
    
  })

  # clear data
  
  observeEvent(input$clear2, {
    output$df_scorecard_2 <- NULL
    
  })
  
  ##############################################################################
  
}
