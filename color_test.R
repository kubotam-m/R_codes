#R color

#Rで定義されている色名と対応する色を調べる
length(colors())
#657
length(colors(distinct=T))
#502

col <- colors(distinct=T)

png("defined_colors.png",width=1400, height=1400)
plot(0:1,0:1,type="n",xlab="",ylab="",ylim=c(23,0),xlim=c(0,23),frame.plot=F,axes=F)
x=0
y=0
i=1
for(y in 0:22){
	for(x in 0:22){
		rect(x,y,x+1,y+1,col=col[i])
		text(x+0.5,y+0.5,labels=i,cex=0.7)
		i = i+1
		if(i>length(colors(distinct=T))){
			break
		}
	}
	if(i>length(colors(distinct=T))){
			break
	}
}
dev.off()
write.table(data.frame(col),file="color_list.txt",sep='\t',quote=F,col.names=F)

#colを数値で指定する
palette()
#[1] "black"   "red"     "green3"  "blue"    "cyan"    "magenta" "yellow"  "gray"   
plot(1:10,rep(0,10),xlab="",ylab="",col=1:10,axes=F)
#8色
png("number_defined_colors.png")
plot(c(0,8),c(0,1),xlim=c(0,8),ylim=c(0,1),
asp=1,axes=F,frame.plot=F,type="n",
xlab="",ylab="")
x=0
for(i in 1:8){
	rect(x,0,x+1,1,col=i)
	text(x+0.5,0.5,labels=i,cex=0.7)
	x = x+1
}
dev.off()

#colorBrewerを使った場合
library(RColorBrewer)
png("colorBrewer_color.png")
display.brewer.all()
dev.off()
png("colorBrewer_ex.png")
barplot(1:10, col=brewer.pal(10,"Spectral"),names.arg=1:10)
dev.off()

#グレースケール
png("gray_scale.png")
plot(c(0,1),c(0,1),type="n",
	xlim=c(0,1), ylim=c(0,1), axes=F, frame.plot=F, asp=0.1,
	xlab="", ylab="")
for(i in 0:100/100){
	rect(i,0,i+1/100,1,col=gray(i),border="transparent")
}
dev.off()

#rgbによる指定
png("rgb_black_scale.png")
plot(c(0,1),c(0,3),type="n",
	xlim=c(0,1), ylim=c(3,0), axes=F, frame.plot=F, asp=1/9,
	xlab="", ylab="")
for(i in 0:100/100){
	rect(i,0,i+1/100,1,col=rgb(i,0,0),border="transparent")
}
for(i in 0:100/100){
	rect(i,1,i+1/100,2,col=rgb(0,i,0),border="transparent")
}
for(i in 0:100/100){
	rect(i,2,i+1/100,3,col=rgb(0,0,i),border="transparent")
}
dev.off()

png("rgb_white_scale.png")
plot(c(0,1),c(0,3),type="n",
	xlim=c(0,1), ylim=c(3,0), axes=F, frame.plot=F, asp=1/9,
	xlab="", ylab="")
for(i in 0:100/100){
	rect(i,0,i+1/100,1,col=rgb(i,1,1),border="transparent")
}
for(i in 0:100/100){
	rect(i,1,i+1/100,2,col=rgb(1,i,1),border="transparent")
}
for(i in 0:100/100){
	rect(i,2,i+1/100,3,col=rgb(1,1,i),border="transparent")
}
dev.off()

#hsvによる指定
png("hsv_scale.png")
plot(c(0,1),c(0,3),type="n",
	xlim=c(0,1), ylim=c(3,0), axes=F, frame.plot=F, asp=1/9,
	xlab="", ylab="")
for(i in 0:100/100){
	rect(i,0,i+1/100,1,col=hsv(i,1,1),border="transparent")
}
for(i in 0:100/100){
	rect(i,1,i+1/100,2,col=hsv(1,i,1),border="transparent")
}
for(i in 0:100/100){
	rect(i,2,i+1/100,3,col=hsv(1,1,i),border="transparent")
}
dev.off()

#colorRampによる指定
pal1 <- colorRamp(c("blue", "red"), space="rgb")
pal2 <- colorRamp(c("blue","white","red"),space="rgb")
pal3 <- colorRamp(c("blue","green","yellow","orange","red"),space="rgb")

png("colorRamp_ex1.png")
plot(c(0,1),c(0,1),type="n",
	xlim=c(0,1), ylim=c(3,0), axes=F, frame.plot=F, asp=0.1,
	xlab="", ylab="")
for(i in 0:100/100){
	tmp_col = pal1(i)/255
	rect(i,0,i+1/100,1,col=rgb(tmp_col[[1]], tmp_col[[2]], tmp_col[[3]]),border="transparent")
}
for(i in 0:100/100){
	tmp_col = pal2(i)/255
	rect(i,1,i+1/100,2,col=rgb(tmp_col[[1]], tmp_col[[2]], tmp_col[[3]]),border="transparent")
}
for(i in 0:100/100){
	tmp_col = pal3(i)/255
	rect(i,2,i+1/100,3,col=rgb(tmp_col[[1]], tmp_col[[2]], tmp_col[[3]]),border="transparent")
}
dev.off()

pal4 <- colorRampPalette(c("blue","green","yellow","orange","red"),space="rgb")
col_l <- pal4(25)

png("colorRamp_ex2.png")
barplot(1:25, names=1:25,col=col_l)
dev.off()
