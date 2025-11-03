#Pruebas del t.test

x=rnorm(20,mean=2,sd=1)
t.test(x) #one-sample t.test
y=rnorm(24,mean=4,sd=2)
t.test(x,y,alternative="two.sided") #dos colas
t.test(x,y,alternative="greater") # x > y
t.test(x,y,alternative="less") # x < y
shapiro.test(x)

#t.test pareado

x=rnorm(20,mean=2,sd=1)
y=rnorm(20,mean=4,sd=2)
t.test(x,y,paired=TRUE)

#anova

#anova
x=rnorm(20,mean=2,sd=1)
y=rnorm(20,mean=4,sd=2)
z=rnorm(20,mean=0,sd=1)
zz=c(x,y,z)
g<-as.factor(c(rep('g1', 20), rep('g2', 20),rep('g3', 20)))
model=aov(zz~g)
anova(model)
#t.testy anovason equivalente
t.test(x,y,var.equal=TRUE)
anova(aov(c(x,y) ~as.factor(c(rep('g1', 20), rep('g2', 20)))))

#t.testvs wilcoxon
x=rnorm(20,mean=2,sd=1)
y=rnorm(20,mean=4,sd=2)
x[1] = 20
boxplot(x,y)
t.test(x,y)
wilcox.test(x,y)


#anovavs k-w
x=rnorm(20,mean=2,sd=1)
y=rnorm(20,mean=4,sd=2)
z=rnorm(20,mean=6,sd=1)
x[1] = 35
y[1] = 0;y[2] = 40
z[1] = 1
boxplot(x,y,z)
zz=c(x,y,z)
g<-as.factor(c(rep('g1', 20), rep('g2', 20),rep('g3', 20)))
model=aov(zz~g)
anova(model)
kruskal.test(zz~g)



## EJERCICIO 1
# 1) Cargar datos
df <- read.csv("season-1819_csv.csv", stringsAsFactors = FALSE)

# 2) Función auxiliar: arma tablas para un equipo dado
team_tables <- function(df, team_name) {
  # Partidos del equipo como local y visitante
  m_home <- subset(df, HomeTeam == team_name)
  m_away <- subset(df, AwayTeam == team_name)
  m_home$Venue <- "Home"
  m_away$Venue <- "Away"
  m <- rbind(m_home, m_away)
  
  # (A) Tabla 2x3: Venue x FTR (H/D/A del dataset)
  tab_2x3 <- with(m, table(Venue, FTR))
  # Reordenar columnas por claridad
  tab_2x3 <- tab_2x3[, c("H","D","A")]
  
  # (B) Crear variable "Win" desde la perspectiva del equipo:
  # - Si jugó de local, gana cuando FTR == "H"
  # - Si jugó de visita, gana cuando FTR == "A"
  m$Win <- ifelse(m$Venue == "Home" & m$FTR == "H", 1,
                  ifelse(m$Venue == "Away" & m$FTR == "A", 1, 0))
  m$Win <- factor(m$Win, levels = c(1,0), labels = c("Win","NoWin"))
  
  # (C) Tabla 2x2: Win vs Venue (para el test de asociación)
  tab_2x2 <- with(m, table(Win, Venue))
  tab_2x2 <- tab_2x2[c("Win","NoWin"), c("Home","Away")]  # ordenar filas/columnas
  
  list(data = m, tab_FTR = tab_2x3, tab_WinVenue = tab_2x2)
}

# 3) Aplicar a Real Sociedad (en el csv aparece como "Sociedad") y Barcelona
soc <- team_tables(df, "Sociedad")
fcb <- team_tables(df, "Barcelona")

# 4) Mostrar tablas 2x3 (Venue x FTR)
soc$tab_FTR
fcb$tab_FTR

# 5) Mostrar tablas 2x2 (Win vs Venue)
soc$tab_WinVenue
fcb$tab_WinVenue

# 6) Test de asociación Win ~ Venue
#    Usamos Fisher por seguridad (funciona bien con conteos pequeños).
#    También puedes ver chisq.test(tab_2x2, correct=TRUE) para el chi-cuadrado con corrección de Yates.
soc_fisher <- fisher.test(soc$tab_WinVenue)      # Real Sociedad
fcb_fisher <- fisher.test(fcb$tab_WinVenue)      # Barcelona

soc_chisq  <- chisq.test(soc$tab_WinVenue, correct = TRUE)
fcb_chisq  <- chisq.test(fcb$tab_WinVenue, correct = TRUE)

soc_fisher
soc_chisq

fcb_fisher
fcb_chisq
