#setwd("/Users/rmizuta/Documents/neta/LRA")
source("http://bit.ly/latent_rank")

#ほぼこれ
#https://www.slideshare.net/simizu706/latent-rank-44708240
df=read.csv("Pokemon.csv")
name=df[1:166, "Name"]
#カラムの確認
names(df)

#使用するカラムの指定
#第一世代のみ（メガあり）
columnList <- c("HP","Attack","Defense","Sp..Atk","Sp..Def","Speed")
df <- df[1:167, columnList]

#最適なrankを探索
for (i in 2:10){
  print(i)
  result <- LRA(df,i)
  AIC(result)
}

#このときはAICが最小になるのは6
result <- LRA(df,6)

#結果の確認
summary(result)

#主成分得点の確認
plot(result)

#各サンプルの所属確率を保存
r1=c()
r2=c()
r3=c()
r4=c()
r5=c()
r6=c()
rank=c()
for ( i in 1:166){
  r1 <- c(r1,result$res[i,1])
  r2 <- c(r2,result$res[i,2])
  r3 <- c(r3,result$res[i,3])
  r4 <- c(r4,result$res[i,4])
  r5 <- c(r5,result$res[i,5])
  r6 <- c(r6,result$res[i,6])
  rank <- c(rank,result$rank[i])
}

out=data.frame(name,r1,r2,r3,r4,r5,r6,rank)

#ファイル書き込み
write.csv(out, "lraresult.csv")

#ブースターの所属確率
plot(result$res[148,],type="l")

#ゲンガーの所属確率
plot(result$res[102,],type="l")

