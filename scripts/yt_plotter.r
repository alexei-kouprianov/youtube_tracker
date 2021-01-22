# setwd("/home/tinea/Documents/H_et_S/Projects/github/youtube_tracker/scripts")

yt.data <- read.table("../data/youtube.txt", h=TRUE, sep="\t")
yt.down <- read.table("../downloads/pages.processed.02.txt", sep="\t")

colnames(yt.down) <- colnames(yt.data)

yt <- rbind.data.frame(yt.data, yt.down)

yt$TIME <- strptime(yt$TIMESTAMP, format="%Y-%m-%d %H:%M:%S")

yt.ls <- as.list(NULL)
yt.down.ls <- as.list(NULL)

for(i in 1:length(levels(yt$TITLE))){
	yt.ls[[i]] <- subset(yt, yt$TITLE == levels(yt$TITLE)[i])
	yt.down.ls[[i]] <- subset(yt.down, yt.down$TITLE == levels(yt.down$TITLE)[i])
	}

axis.1.at <- seq(
	from = strptime("2020-12-14 00:00", "%Y-%m-%d %H:%M"),
	to = strptime("2021-01-31 00:00", "%Y-%m-%d %H:%M"),
	by = "day")

axis.base.counts <- ceiling(max(yt$COUNTS, na.rm=TRUE)/(2e6))
axis.base.likes <- ceiling(max(yt$LIKES, na.rm=TRUE)/(2e5))
axis.base.dislikes <- ceiling(max(yt$DISLIKES, na.rm=TRUE)/(1e4))

################################################################
# Time series plots
################################################################

png("../plots/youtube.2020-12-XX.COUNTS.png", height=750, width=750)
par(mar=c(8,5,5,2)+.1)

plot(
	yt.ls[[1]]$TIME,
	yt.ls[[1]]$COUNTS,
	ylim=c(0, max(yt$COUNTS)),
	main="Alexei Navalny channel movies:\nA comparison of impact",
	xlab="", ylab="Views",
	type="n", axes=FALSE)

axis(2, at=(0:axis.base.counts)*2*1000000, labels=c(0, paste((1:axis.base.counts)*2, "М", sep="")))

# axis.POSIXct(1,
# 	at = seq(
# 	from = strptime("2020-12-14 00:00", format="%Y-%m-%d %H:%M"),
# 	to = strptime("2021-01-31 00:00", format="%Y-%m-%d %H:%M"),
# 	by = "hour"),
# 	labels = FALSE, tcl = -.25,
# 	las = 2)

axis.POSIXct(1,
	at = axis.1.at,
	format = "%Y-%m-%d %H:%M",
	las = 2)

abline(h=(0:axis.base.counts)*2*1000000,
	v=seq(from = strptime("2020-12-14 12:00", format="%Y-%m-%d %H:%M"),
	to = strptime("2021-01-31 12:00", format="%Y-%m-%d %H:%M"),
	by = "day"), col=8, lty=3)
# abline(v=seq(from = strptime("2020-12-14 06:00", format="%Y-%m-%d %H:%M"),
# 	to = strptime("2021-01-31 06:00", format="%Y-%m-%d %H:%M"),
# 	by = "day"), col=8, lty=3)
# abline(v=seq(from = strptime("2020-12-14 18:00", format="%Y-%m-%d %H:%M"),
# 	to = strptime("2021-01-31 18:00", format="%Y-%m-%d %H:%M"),
# 	by = "day"), col=8, lty=3)
abline(v=seq(from = strptime("2020-12-14 00:00", format="%Y-%m-%d %H:%M"),
	to = strptime("2021-01-31 00:00", format="%Y-%m-%d %H:%M"),
	by = "day"), col=8, lty=5, lwd=1.5)

points(
	yt.ls[[1]]$TIME,
	yt.ls[[1]]$COUNTS,
	type="o", pch=20, cex=.5, col=rgb(0,0,0,.3))

points(
	yt.ls[[2]]$TIME,
	yt.ls[[2]]$COUNTS,
	type="o", pch=20, cex=.5, col=rgb(0,0,1,.3))

points(
	yt.ls[[7]]$TIME,
	yt.ls[[7]]$COUNTS,
	type="o", pch=20, cex=.5, col=rgb(1,0,0,.3))

points(
	yt.ls[[3]]$TIME,
	yt.ls[[3]]$COUNTS,
	type="o", pch=20, cex=.5, col=rgb(1,.8,.2,.3))

points(
	yt.ls[[6]]$TIME,
	yt.ls[[6]]$COUNTS,
	type="o", pch=20, cex=.5, col=rgb(0,1,0,.3))

points(
	yt.ls[[4]]$TIME,
	yt.ls[[4]]$COUNTS,
	type="o", pch=20, cex=.5, col=rgb(.5,0,0,.3))

points(
	yt.ls[[5]]$TIME,
	yt.ls[[5]]$COUNTS,
	type="o", pch=20, cex=.5, col=rgb(1,0,1,.3))

legend(
	"topleft",
	bty="o",
	box.lty = 0,
	bg = rgb(1,1,1,.3),
	lty=1,
	lwd=4,
	col=rgb(
		c(0,0,1,0,1,.5,1),
		c(0,0,0,1,.8,.0,0),
		c(0,1,0,0,.2,.0,1),
		c(.5,.5,.5,.5,.5,.5,.5)),

# levels(yt$TITLE)
# [1] "Дело раскрыто. Я знаю всех, кто пытался меня убить"
# [2] "Я позвонил своему убийце. Он признался"
# [3] "«У них нет моральной границы»: расследователь Христо Грозев об отравлении Алексея Навального"
# [4] "Дворец для Путина. История самой большой взятки"
# [5] "Он вам не Димон"
# [6] "Президент Навальный: как он ослабит Москву, заставит платить миллиардеров и кому поднимет зарплаты"
# [7] "Уголовное дело против Любови Соболь. Первый комментарий после освобождения"

	legend=paste(
		c("I know who wanted to kill me (",
		"I've called my assassin (",
		"A criminal case against Sobol' (",
		"Guriev interviews Navalny (",
		"Grozev: They know no moral limits (",
		"Putin's palace (",
		"Don't call him Dimon ("),
		c(round(yt.ls[[1]]$COUNTS[nrow(yt.ls[[1]])]/1e6, 3),
		round(yt.ls[[2]]$COUNTS[nrow(yt.ls[[2]])]/1e6, 3),
		round(yt.ls[[7]]$COUNTS[nrow(yt.ls[[7]])]/1e6, 3),
		round(yt.ls[[6]]$COUNTS[nrow(yt.ls[[6]])]/1e6, 3),
		round(yt.ls[[3]]$COUNTS[nrow(yt.ls[[3]])]/1e6, 3),
		round(yt.ls[[4]]$COUNTS[nrow(yt.ls[[4]])]/1e6, 3),
		round(yt.ls[[5]]$COUNTS[nrow(yt.ls[[5]])]/1e6, 3)
		),
		rep("M)", 7), sep="")
	)

dev.off()

################################################################
# Likes

png("../plots/youtube.2020-12-XX.LIKES.png", height=750, width=750)
par(mar=c(8,5,5,2)+.1)

plot(
	yt.ls[[1]]$TIME,
	yt.ls[[1]]$LIKES,
	ylim=c(0, max(yt$LIKES, na.rm=TRUE)),
	main="Alexei Navalny channel movies:\nA comparison of impact",
	xlab="", ylab="Likes",
	type="n", axes=FALSE)

abline(h=(0:axis.base.likes)*2*100000,
	v=seq(from = strptime("2020-12-14 12:00", format="%Y-%m-%d %H:%M"),
	to = strptime("2021-01-31 12:00", format="%Y-%m-%d %H:%M"),
	by = "day"), col=8, lty=3)
# abline(v=seq(from = strptime("2020-12-14 06:00", format="%Y-%m-%d %H:%M"),
# 	to = strptime("2021-01-31 06:00", format="%Y-%m-%d %H:%M"),
# 	by = "day"), col=8, lty=3)
# abline(v=seq(from = strptime("2020-12-14 18:00", format="%Y-%m-%d %H:%M"),
# 	to = strptime("2021-01-31 18:00", format="%Y-%m-%d %H:%M"),
# 	by = "day"), col=8, lty=3)
abline(v=seq(from = strptime("2020-12-14 00:00", format="%Y-%m-%d %H:%M"),
	to = strptime("2021-01-31 00:00", format="%Y-%m-%d %H:%M"),
	by = "day"), col=8, lty=5, lwd=1.5)

points(
	yt.ls[[1]]$TIME,
	yt.ls[[1]]$LIKES,
	type="o", pch=20, cex=.5, col=rgb(0,0,0,.3))

points(
	yt.ls[[2]]$TIME,
	yt.ls[[2]]$LIKES,
	type="o", pch=20, cex=.5, col=rgb(0,0,1,.3))

points(
	yt.ls[[7]]$TIME,
	yt.ls[[7]]$LIKES,
	type="o", pch=20, cex=.5, col=rgb(1,0,0,.3))

points(
	yt.ls[[3]]$TIME,
	yt.ls[[3]]$LIKES,
	type="o", pch=20, cex=.5, col=rgb(1,.8,.2,.3))

points(
	yt.ls[[6]]$TIME,
	yt.ls[[6]]$LIKES,
	type="o", pch=20, cex=.5, col=rgb(0,1,0,.3))

points(
	yt.ls[[4]]$TIME,
	yt.ls[[4]]$LIKES,
	type="o", pch=20, cex=.5, col=rgb(.5,0,0,.3))

points(
	yt.ls[[5]]$TIME,
	yt.ls[[5]]$LIKES,
	type="o", pch=20, cex=.5, col=rgb(1,0,1,.3))

legend(
	"topleft",
	bty="o",
	box.lty = 0,
	bg = rgb(1,1,1,.3),
	lty=1,
	lwd=4,
	col=rgb(
		c(0,0,1,0,1,.5,1),
		c(0,0,0,1,.8,0,0),
		c(0,1,0,0,.2,0,1),
		c(.5,.5,.5,.5,.5,.5,.5)),

	legend=paste(
		c("I know who wanted to kill me (",
		"I've called my assassin (",
		"A criminal case against Sobol' (",
		"Guriev interviews Navalny (",
		"Grozev: They know no moral limits (",
		"Putin's palace (",
		"Don't call him Dimon ("),
		c(round(yt.ls[[1]]$LIKES[nrow(yt.ls[[1]])]/1e6, 3),
		round(yt.ls[[2]]$LIKES[nrow(yt.ls[[2]])]/1e6, 3),
		round(yt.ls[[7]]$LIKES[nrow(yt.ls[[7]])]/1e6, 3),
		round(yt.ls[[6]]$LIKES[nrow(yt.ls[[6]])]/1e6, 3),
		round(yt.ls[[3]]$LIKES[nrow(yt.ls[[3]])]/1e6, 3),
		round(yt.ls[[4]]$LIKES[nrow(yt.ls[[4]])]/1e6, 3),
		round(yt.ls[[5]]$LIKES[nrow(yt.ls[[5]])]/1e6, 3)
		),
		rep("M)", 7), sep="")
	)

axis.POSIXct(1,
	at = axis.1.at,
	format = "%Y-%m-%d %H:%M",
	las = 2)

axis(2, at=(0:axis.base.likes)*2*100000, labels=c(0, paste((1:axis.base.likes)*2, "00K", sep="")))

dev.off()

################################################################
# Dislikes

png("../plots/youtube.2020-12-XX.DISLIKES.png", height=750, width=750)
par(mar=c(8,5,5,2)+.1)

plot(
	yt.ls[[1]]$TIME,
	yt.ls[[1]]$DISLIKES,
	ylim=c(0, max(yt$DISLIKES, na.rm=TRUE)),
	main="Alexei Navalny channel movies:\nA comparison of impact",
	xlab="", ylab="Disikes",
	type="n", axes=FALSE)

abline(h=(0:axis.base.dislikes)*10000,
	v=seq(from = strptime("2020-12-14 12:00", format="%Y-%m-%d %H:%M"),
	to = strptime("2021-01-31 12:00", format="%Y-%m-%d %H:%M"),
	by = "day"), col=8, lty=3)
abline(v=seq(from = strptime("2020-12-14 00:00", format="%Y-%m-%d %H:%M"),
	to = strptime("2021-01-31 00:00", format="%Y-%m-%d %H:%M"),
	by = "day"), col=8, lty=5, lwd=1.5)

points(
	yt.ls[[1]]$TIME,
	yt.ls[[1]]$DISLIKES,
	type="o", pch=20, cex=.5, col=rgb(0,0,0,.3))

points(
	yt.ls[[2]]$TIME,
	yt.ls[[2]]$DISLIKES,
	type="o", pch=20, cex=.5, col=rgb(0,0,1,.3))

points(
	yt.ls[[7]]$TIME,
	yt.ls[[7]]$DISLIKES,
	type="o", pch=20, cex=.5, col=rgb(1,0,0,.3))

points(
	yt.ls[[3]]$TIME,
	yt.ls[[3]]$DISLIKES,
	type="o", pch=20, cex=.5, col=rgb(1,.8,.2,.3))

points(
	yt.ls[[6]]$TIME,
	yt.ls[[6]]$DISLIKES,
	type="o", pch=20, cex=.5, col=rgb(0,1,0,.3))

points(
	yt.ls[[4]]$TIME,
	yt.ls[[4]]$DISLIKES,
	type="o", pch=20, cex=.5, col=rgb(.5,0,0,.3))

points(
	yt.ls[[5]]$TIME,
	yt.ls[[5]]$DISLIKES,
	type="o", pch=20, cex=.5, col=rgb(1,0,1,.3))

legend(
	"bottomleft",
	bty="o",
	box.lty = 0,
	bg = rgb(1,1,1,.3),
	lty=1,
	lwd=4,
	col=rgb(
		c(0,0,1,0,1,.5,1),
		c(0,0,0,1,.8,0,0),
		c(0,1,0,0,.2,0,1),
		c(.5,.5,.5,.5,.5,.5,.5)),

	legend=paste(
		c("I know who wanted to kill me (",
		"I've called my assassin (",
		"A criminal case against Sobol' (",
		"Guriev interviews Navalny (",
		"Grozev: They know no moral limits (",
		"Putin's palace (",
		"Don't call him Dimon ("),
		c(round(yt.ls[[1]]$DISLIKES[nrow(yt.ls[[1]])]/1e3, 1),
		round(yt.ls[[2]]$DISLIKES[nrow(yt.ls[[2]])]/1e3, 1),
		round(yt.ls[[7]]$DISLIKES[nrow(yt.ls[[7]])]/1e3, 1),
		round(yt.ls[[6]]$DISLIKES[nrow(yt.ls[[6]])]/1e3, 1),
		round(yt.ls[[3]]$DISLIKES[nrow(yt.ls[[3]])]/1e3, 1),
		round(yt.ls[[4]]$DISLIKES[nrow(yt.ls[[4]])]/1e3, 1),
		round(yt.ls[[5]]$DISLIKES[nrow(yt.ls[[5]])]/1e3, 1)
		),
		rep("K)", 7), sep="")
	)

axis.POSIXct(1,
	at = axis.1.at,
	format = "%Y-%m-%d %H:%M",
	las = 2)

axis(2, at=(0:axis.base.dislikes)*10000, labels=c(0, paste((1:axis.base.dislikes), "0K", sep="")))

dev.off()

################################################################
# Scatter plots
################################################################

png("../plots/youtube.2020-12-XX.scatter.COUNTS_LIKES.png", height=750, width=750)
par(mar=c(8,5,5,2)+.1)

plot(
	yt.ls[[1]]$COUNTS,
	yt.ls[[1]]$LIKES,
	xlim=c(0, max(yt$COUNTS, na.rm=TRUE)),
	ylim=c(0, max(yt$LIKES, na.rm=TRUE)),
	main="Alexei Navalny channel movies:\nA comparison of impact",
	xlab="Counts", ylab="Likes",
	type="n", axes=FALSE)

abline(h=(0:axis.base.likes)*2*100000, v=(0:axis.base.counts)*2*1000000, col=rgb(0,0,0,.3), lty=3)

points(
	yt.ls[[1]]$COUNTS,
	yt.ls[[1]]$LIKES,
	pch=20, cex=.5, col=rgb(0,0,0,.3))

points(
	yt.ls[[2]]$COUNTS,
	yt.ls[[2]]$LIKES,
	pch=20, cex=.5, col=rgb(0,0,1,.3))

points(
	yt.ls[[7]]$COUNTS,
	yt.ls[[7]]$LIKES,
	pch=20, cex=.5, col=rgb(1,0,0,.3))

points(
	yt.ls[[6]]$COUNTS,
	yt.ls[[6]]$LIKES,
	pch=20, cex=.5, col=rgb(0,1,0,.3))

points(
	yt.ls[[4]]$COUNTS,
	yt.ls[[4]]$LIKES,
	pch=20, cex=.5, col=rgb(.5,0,0,.3))

points(
	yt.ls[[3]]$COUNTS,
	yt.ls[[3]]$LIKES,
	pch=20, cex=.5, col=rgb(1,.8,.2,.3))

points(
	yt.ls[[5]]$COUNTS,
	yt.ls[[5]]$LIKES,
	type="o", pch=20, cex=.5, col=rgb(1,0,1,.3))

legend(
	"bottomright",
	bty="o",
	box.lty = 0,
	bg = rgb(1,1,1,.3),
	pch=16,
	cex=1,
	col=rgb(
		c(0,0,1,0,1,.5,1),
		c(0,0,0,1,.8,0,0),
		c(0,1,0,0,.2,0,1),
		c(.5,.5,.5,.5,.5,.5,.5)),

# levels(yt$TITLE)
# [1] "Дело раскрыто. Я знаю всех, кто пытался меня убить"
# [2] "Я позвонил своему убийце. Он признался"
# [3] "«У них нет моральной границы»: расследователь Христо Грозев об отравлении Алексея Навального"
# [4] "Дворец для Путина. История самой большой взятки"
# [5] "Президент Навальный: как он ослабит Москву, заставит платить миллиардеров и кому поднимет зарплаты"
# [6] "Уголовное дело против Любови Соболь. Первый комментарий после освобождения"

	legend=paste(
		c("I know who wanted to kill me (",
		"I've called my assassin (",
		"A criminal case against Sobol' (",
		"Guriev interviews Navalny (",
		"Grozev: They know no moral limits (",
		"Putin's palace (",
		"Don't call him Dimon ("),
		c(round(yt.ls[[1]]$COUNTS[nrow(yt.ls[[1]])]/1e6, 3),
		round(yt.ls[[2]]$COUNTS[nrow(yt.ls[[2]])]/1e6, 3),
		round(yt.ls[[7]]$COUNTS[nrow(yt.ls[[7]])]/1e6, 3),
		round(yt.ls[[6]]$COUNTS[nrow(yt.ls[[6]])]/1e6, 3),
		round(yt.ls[[3]]$COUNTS[nrow(yt.ls[[3]])]/1e6, 3),
		round(yt.ls[[4]]$COUNTS[nrow(yt.ls[[4]])]/1e6, 3),
		round(yt.ls[[4]]$COUNTS[nrow(yt.ls[[5]])]/1e6, 3)
		),
		rep(" / ", 7),
		c(round(yt.ls[[1]]$LIKES[nrow(yt.ls[[1]])]/1e6, 3),
		round(yt.ls[[2]]$LIKES[nrow(yt.ls[[2]])]/1e6, 3),
		round(yt.ls[[7]]$LIKES[nrow(yt.ls[[7]])]/1e6, 3),
		round(yt.ls[[6]]$LIKES[nrow(yt.ls[[6]])]/1e6, 3),
		round(yt.ls[[3]]$LIKES[nrow(yt.ls[[3]])]/1e6, 3),
		round(yt.ls[[4]]$LIKES[nrow(yt.ls[[4]])]/1e6, 3),
		round(yt.ls[[5]]$LIKES[nrow(yt.ls[[5]])]/1e6, 3)
		),
		rep("M)", 7), sep="")
	)

axis(1, at=(0:axis.base.counts)*2*1000000, labels=c(0, paste((1:axis.base.counts)*2, "M", sep="")))
axis(2, at=(0:axis.base.likes)*2*100000, labels=c(0, paste((1:axis.base.likes)*2, "00K", sep="")))

dev.off()

png("../plots/youtube.2020-12-XX.scatter.LIKES_DISLIKES.png", height=750, width=750)
par(mar=c(8,5,5,2)+.1)

plot(
	yt.ls[[1]]$LIKES,
	yt.ls[[1]]$DISLIKES,
	xlim=c(0, max(yt$LIKES, na.rm=TRUE)),
	ylim=c(0, max(yt$DISLIKES, na.rm=TRUE)),
	main="Alexei Navalny channel movies:\nA comparison of impact",
	xlab="Likes", ylab="Dislikes",
	type="n", axes=FALSE)

abline(v=(0:axis.base.likes)*2*100000, h=(0:axis.base.dislikes)*10000, col=rgb(0,0,0,.3), lty=3)

points(
	yt.ls[[1]]$LIKES,
	yt.ls[[1]]$DISLIKES,
	pch=20, cex=.5, col=rgb(0,0,0,.3))

points(
	yt.ls[[2]]$LIKES,
	yt.ls[[2]]$DISLIKES,
	pch=20, cex=.5, col=rgb(0,0,1,.3))

points(
	yt.ls[[7]]$LIKES,
	yt.ls[[7]]$DISLIKES,
	pch=20, cex=.5, col=rgb(1,0,0,.3))

points(
	yt.ls[[6]]$LIKES,
	yt.ls[[6]]$DISLIKES,
	pch=20, cex=.5, col=rgb(0,1,0,.3))

points(
	yt.ls[[4]]$LIKES,
	yt.ls[[4]]$DISLIKES,
	pch=20, cex=.5, col=rgb(.5,0,0,.3))

points(
	yt.ls[[3]]$LIKES,
	yt.ls[[3]]$DISLIKES,
	pch=20, cex=.5, col=rgb(1,.8,.2,.3))

points(
	yt.ls[[5]]$LIKES,
	yt.ls[[5]]$DISLIKES,
	type="o", pch=20, cex=.5, col=rgb(1,0,1,.3))

legend(
	"bottomright",
	bty="o",
	box.lty = 0,
	bg = rgb(1,1,1,.3),
	pch=16,
	cex=1,
	col=rgb(
		c(0,0,1,0,1,.5,1),
		c(0,0,0,1,.8,0,0),
		c(0,1,0,0,.2,0,1),
		c(.5,.5,.5,.5,.5,.5,.5)),

# levels(yt$TITLE)
# [1] "Дело раскрыто. Я знаю всех, кто пытался меня убить"
# [2] "Я позвонил своему убийце. Он признался"
# [3] "«У них нет моральной границы»: расследователь Христо Грозев об отравлении Алексея Навального"
# [4] "Дворец для Путина. История самой большой взятки"
# [5] "Президент Навальный: как он ослабит Москву, заставит платить миллиардеров и кому поднимет зарплаты"
# [6] "Уголовное дело против Любови Соболь. Первый комментарий после освобождения"

	legend=paste(
		c("I know who wanted to kill me (",
		"I've called my assassin (",
		"A criminal case against Sobol' (",
		"Guriev interviews Navalny (",
		"Grozev: They know no moral limits (",
		"Putin's palace (",
		"Don't call him Dimon ("),
		c(round(yt.ls[[1]]$DISLIKES[nrow(yt.ls[[1]])]/1e3, 1),
		round(yt.ls[[2]]$DISLIKES[nrow(yt.ls[[2]])]/1e3, 1),
		round(yt.ls[[7]]$DISLIKES[nrow(yt.ls[[7]])]/1e3, 1),
		round(yt.ls[[6]]$DISLIKES[nrow(yt.ls[[6]])]/1e3, 1),
		round(yt.ls[[3]]$DISLIKES[nrow(yt.ls[[3]])]/1e3, 1),
		round(yt.ls[[4]]$DISLIKES[nrow(yt.ls[[4]])]/1e3, 1),
		round(yt.ls[[4]]$DISLIKES[nrow(yt.ls[[5]])]/1e3, 1)
		),
		rep(" / ", 7),
		c(round(yt.ls[[1]]$LIKES[nrow(yt.ls[[1]])]/1e3, 1),
		round(yt.ls[[2]]$LIKES[nrow(yt.ls[[2]])]/1e3, 1),
		round(yt.ls[[7]]$LIKES[nrow(yt.ls[[7]])]/1e3, 1),
		round(yt.ls[[6]]$LIKES[nrow(yt.ls[[6]])]/1e3, 1),
		round(yt.ls[[3]]$LIKES[nrow(yt.ls[[3]])]/1e3, 1),
		round(yt.ls[[4]]$LIKES[nrow(yt.ls[[4]])]/1e3, 1),
		round(yt.ls[[5]]$LIKES[nrow(yt.ls[[5]])]/1e3, 1)
		),
		rep("K)", 7), sep="")
	)

axis(1, at=(0:axis.base.likes)*2*100000, labels=c(0, paste((1:axis.base.likes)*2, "00K", sep="")))
axis(2, at=(0:axis.base.dislikes)*10000, labels=c(0, paste((1:axis.base.dislikes), "0K", sep="")))

dev.off()

# source("yt_plotter.r")
print(yt.ls[[1]]$COUNTS[nrow(yt.ls[[1]])] - yt.ls[[2]]$COUNTS[nrow(yt.ls[[2]])])
