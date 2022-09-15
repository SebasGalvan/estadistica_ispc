# LIBS -------------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(ggrepel)
library(ggpubr)


# CARGA ------------------------------------------------------------------------

df_alumnos <- read_excel("./data/alumnos.xlsx")


nom_ape <- str_c(df_alumnos$nombre," " ,df_alumnos$apellido)
tri_1 <- df_alumnos$nota1
tri_2 <- df_alumnos$nota2
tri_3 <- df_alumnos$nota3
id <- df_alumnos$id
curso <- df_alumnos$curso
modulo <- df_alumnos$modulo


df_f = data.frame(
  id,
  nom_ape,
  curso,
  modulo,
  tri_1,
  tri_2,
  tri_3
)

# TRANSPOSICION ----------------------------------------------------------------
pivot_t <- gather(df_f, "trimestre", "nota", 5:7)
df_ordenado <- pivot_t[order(pivot_t$id, pivot_t$curso), ]

# FILTRO -----------------------------------------------------------------------
primero <- filter(df_ordenado, curso == 1)
segundo <- filter(df_ordenado, curso == 2)
tercero <- filter(df_ordenado, curso == 3)
cuarto <- filter(df_ordenado, curso == 4)
quinto <- filter(df_ordenado, curso == 5)

# SCATERPLOT ------------------------------------------------------------------------------

primer_tri <- filter(pivot_t ,  trimestre == "tri_1")
ord <- sort(pivot_t$nota)
fa <- table(ord)
fr <- fa / length(pivot_t[,1])

com <- data.frame(
  "frec" = fr
  )

ggplot(com, aes(x= frec.ord  , y= frec.Freq)) + 
  geom_point(fill="blue", color="darkred")+
  geom_text(label=round(com$frec.Freq, 3), vjust=1.1)+
  ggtitle("Frecuencia de notas de todos los trimestres")+
  labs(y = "Frecuencia de nota", x = "Notas")+
  theme_minimal()




# BARPLOT ----------------------------------------------------------------------------------

# display.brewer.all()
  
promedios <- c()

  mod_nom <- c("Comprension auditiva","Escritura", "Expresion ORAL", "Lectura")
  modulos_db <- factor(mod_nom, levels = mod_nom)
  
  for(i in modulos_db){
    for(j in 1:5){
      n_m_c <- filter(pivot_t, modulo == i, curso == j)
      prom <- mean(n_m_c$nota)
      promedios <- c(promedios, prom)
    }
  }
  


  modulos <- modulos_db
  c_nom <-  c("Primero", "Segundo", "Tercero", "Cuarto", "Quinto")
  curso_name <- factor(c_nom , levels = c_nom)
  promedios <- promedios

  data <- data.frame(modulos, curso_name, promedios)

  # Grouped
  ggplot(data, aes(fill=curso_name, y=promedios, x=modulos)) +
    geom_bar(position="dodge", stat="identity")+
    guides(fill=guide_legend(title="Cursos"))+
    labs(y = "Promedios por modulo", x = "")+
    theme_light()+
    # scale_fill_brewer(palette = "Spectral")
    scale_fill_manual(values = c("#003049", "#d62828","#f77f00","#fcbf49","#eae2b7"))


  

# LINEPLOT ---------------------------------------------------------------------  

  cursos <- list()
  lbls_curso <- c()
  
  # mod <-"Comprension auditiva"
  # mod <-"Escritura"
  # mod <-"Expresion ORAL"
  mod <- "Lectura"
  
  
  pr <- filter(primero, modulo == mod)
  sg <- filter(segundo, modulo == mod)
  tr <- filter(tercero, modulo == mod)
  ct <- filter(cuarto, modulo == mod)
  qt <- filter(quinto, modulo == mod)
  
  cont <- as.logical(c(
    length(pr[, 1]),
    length(sg[, 1]),
    length(tr[, 1]),
    length(ct[, 1]),
    length(qt[, 1])
  ))
  
  len <- unname(table(cont)[2:2])
  
  if (cont[1] == TRUE) {
    
    prom_t1 <- mean(filter(pr, trimestre == "tri_1")$nota)
    prom_t2 <- mean(filter(pr, trimestre == "tri_2")$nota)
    prom_t3 <- mean(filter(pr, trimestre == "tri_3")$nota)
    
    lbls_curso <- c(lbls_curso , "CURSO 1")
    pr$id <- as.factor(pr$id)
    c1 <- ggplot(data = pr, aes(x = trimestre, y = nota, group = id)) +
      geom_line(aes(color = id)) +
      geom_point(aes(color = id)) +
      theme_light() +
      theme(legend.position = "none") +
      geom_line(aes(y = mean(nota)), color = "#2a9d8f", size = 1.2) +
      labs(x = " ") +
      scale_color_manual(values = c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7"))
    
    cursos <- append(cursos, c1)
  }
  
  
  if (cont[2]  == TRUE) {
    sg$id <- as.factor(sg$id)
    lbls_curso <- c(lbls_curso , "CURSO 2")
    c2 <- ggplot(data = sg, aes(x = trimestre, y = nota, group = id)) +
      geom_line(aes(color = id)) +
      geom_point(aes(color = id)) +
      theme_light() +
      theme(legend.position = "none") +
      geom_line(aes(y = mean(nota)), color = "#2a9d8f", size = 1.2) +
      labs(x = " ") +
      scale_color_manual(values = c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7"))
    
    cursos <- append(cursos, c2)
  }
  
  if (cont[3]) {
    lbls_curso <- c(lbls_curso , "CURSO 3")
    tr$id <- as.factor(tr$id)
    c3 <- ggplot(data = tr, aes(x = trimestre, y = nota, group = id)) +
      geom_line(aes(color = id)) +
      geom_point(aes(color = id)) +
      theme_light() +
      theme(legend.position = "none") +
      geom_line(aes(y = mean(nota)), color = "#2a9d8f", size = 1.2) +
      labs(x = " ") +
      scale_color_manual(values = c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7"))
    
    cursos <- append(cursos, c3)
  }
  
  if (cont[4]  == TRUE) {
    lbls_curso <- c(lbls_curso , "CURSO 4")
    ct$id <- as.factor(ct$id)
    c4 <- ggplot(data = ct, aes(x = trimestre, y = nota, group = id)) +
      geom_line(aes(color = id)) +
      geom_point(aes(color = id)) +
      theme_light() +
      theme(legend.position = "none") +
      geom_line(aes(y = mean(nota)), color = "#2a9d8f", size = 1.2) +
      labs(x = " ") +
      scale_color_manual(values = c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7"))
    
    cursos <- append(cursos, c4)
  }
  
  if (cont[5]  == TRUE) {
    lbls_curso <- c(lbls_curso , "CURSO 5")
    qt$id <- as.factor(qt$id)
    c5 <- ggplot(data = qt, aes(x = trimestre, y = nota, group = id)) +
      geom_line(aes(color = id)) +
      geom_point(aes(color = id)) +
      theme_light() +
      theme(legend.position = "none") +
      geom_line(aes(y = mean(nota)), color = "#2a9d8f", size = 1.2) +
      labs(x = " ") +
      scale_color_manual(values = c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7"))
    
    cursos <- append(cursos, c5)
    
  }
  
  lista <- list(if (cont[1]) {
    c1
  },
  if (cont[2]) {
    c2
  },
  if (cont[3]) {
    c3
  },
  if (cont[4]) {
    c4
  },
  if (cont[5]) {
    c5
  })
  
  plot <- ggarrange(
    plotlist = lista,
    labels = lbls_curso,
    hjust = -4.5,
    vjust = 34,
    ncol =  3,
    nrow = 2,
    font.label = list(size = 8, color = "#f77f00", face = "bold"),
    common.legend = TRUE,
    legend = "none"
  )
  
  lbl <-
    str_c("Notas trimestrales de todos  los cursos del modulo: ", mod)
  annotate_figure(plot, top = text_grob(lbl , color = "#003049", size = 10))
  
  
# PIECHART - PIEPLOT --------------------------------------------------------------------

cursos_u <- curso_name
cantidad_cursos <- c(dim(filter(df_alumnos , curso == 1))[1],
                     dim(filter(df_alumnos , curso == 2))[1],
                     dim(filter(df_alumnos , curso == 3))[1],
                     dim(filter(df_alumnos , curso == 4))[1],
                     dim(filter(df_alumnos , curso == 5))[1]
                  
                    )

sum(cantidad_cursos)

df_pie <- data.frame(
  cursos_u,
  cantidad_cursos
) 

  
ggplot(df_pie, aes(x="", y=cantidad_cursos, fill=cursos_u)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  ggtitle("Alumnos por curso")+
  labs(y = "", x = "")+
  theme_void()+
  geom_text(aes(x = 1.6, label = cantidad_cursos),
            position = position_stack(vjust = .5))+
  guides(fill=guide_legend(title="Cursos"))+
  scale_fill_manual(values = c("#003049", "#d62828", "#f77f00", "#fcbf49", "#eae2b7"))

# MEDIDAS DE CONCENTRACIÓN Y DISPERSIÓN ----------------------------------------



notas <- df_ordenado$nota
notas_ordendas <- sort(notas)

media <- mean(notas)
mediana <- median(notas_ordendas)
desvio <- sd(notas)

media 
mediana  
desvio





