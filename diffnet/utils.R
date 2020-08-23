diffNet <- function(df1, df2){
  df_merge <- merge(df1, df2)
  df1_only <- (rbind(df1, df_merge) %>% group_by(V1, V2) %>% filter(n() == 1))
  df2_only <- (rbind(df2, df_merge) %>% group_by(V1, V2) %>% filter(n() == 1))

  positive <- 1
  negative <- -1

  df1_only <- cbind(df1_only, positive)
  df2_only <- cbind(df2_only, negative)

  colnames(df1_only) <- c("V1", "V2", "weight")
  colnames(df2_only) <- c("V1", "V2", "weight")

  df3 <- rbind(df1_only, df2_only)
  df3 <- df3[order(df3$V1), ]
  sum <- list(df1_only, df2_only, df3)
  names(sum) <- c("df1", "df2", "df3")
  return (sum)
}

netces <- function(df1, df2){
  # 引数で与えられたデータをグラフで表現
  gA <- graph.data.frame(df1, directed = F)
  gB <- graph.data.frame(df2, directed = F)
  # グラフgAの各ノードに名前をつける
	if(length(V(gA)$name) == 0){
		warning("Node names are missing in network A. Node indecies were assigned as node names.")
		V(gA)$name <- paste("v",1:vcount(gA),sep="")
	}
  # グラフgBの各ノードに名前をつける
	if(length(V(gB)$name) == 0){
		warning("Node names are missing in network B. Node indecies were assigned as node names.")
		V(gB)$name <- paste("v",1:vcount(gB),sep="")
	}

	#  gAとgBにおける共通のノードを抽出する
	comm_node_set <- intersect(V(gA)$name, V(gB)$name)

  # gAにおける最も共通のサブグラフを取り除く
	del_nodes_A <- V(gA)$name[V(gA)$name %in% comm_node_set == F]
	gA_sub <- delete_vertices(gA, del_nodes_A)

  # gBにおける最も共通のサブグラフを取り除く
	del_nodes_B <- V(gB)$name[V(gB)$name %in% comm_node_set == F]
	gB_sub <- delete_vertices(gB, del_nodes_B)

	# gAの媒介中心性を計算
	bet_gA_sub <- betweenness(gA_sub)
	scaled_bet_gA_sub <- (bet_gA_sub - min(bet_gA_sub)) / (max(bet_gA_sub) - min(bet_gA_sub))
	scaled_bet_gA_sub <- ifelse(is.nan(scaled_bet_gA_sub), 0, scaled_bet_gA_sub)
	# gBの媒介中心性を計算
	bet_gB_sub <- betweenness(gB_sub)
	scaled_bet_gB_sub <- (bet_gB_sub - min(bet_gB_sub)) / (max(bet_gB_sub) - min(bet_gB_sub))
	scaled_bet_gB_sub <- ifelse(is.nan(scaled_bet_gB_sub), 0, scaled_bet_gB_sub)
	#  dBを計算
	dB <- scaled_bet_gB_sub[comm_node_set] - scaled_bet_gA_sub[comm_node_set]

	# gAにおけるノードの次数を計算
	deg_gA_sub <- degree(gA_sub)
	# gBにおけるノードの次数を計算
	deg_gB_sub <- degree(gB_sub)

	# ノード毎にNESHスコアを計算
	nesh <- c()
	max_deg_B <- max(deg_gB_sub)
	for(v in comm_node_set){
		neiA <- neighbors(gA_sub, v) # gAにおける近接ノードを格納
		neiB <- neighbors(gB_sub, v) # gBにおける近接ノードを格納
		interAB <- length(intersect(neiA %>% as_ids(), neiB %>% as_ids())) # neiAとneiBに共通して含まれるノードの数を計算（積集合）
		unionAB <- length(union(neiA %>% as_ids(), neiB %>% as_ids())) # neiAもしくはneiBに含まれるノードの数を計算（和集合）
		diffBA <- length(setdiff(neiB %>% as_ids(), neiA %>% as_ids())) # neiBにのみ含まれるノードの数を計算（差集合）
    # 上記の値を元にNESHスコアを計算
		tmp <- interAB / unionAB - (diffBA / max_deg_B + diffBA / unionAB)
		nesh <- c(nesh, 1 - tmp)
	}

	nesh[is.nan(nesh)] <- 0
	names(nesh) <- comm_node_set
	sum <- list(round(nesh, 2), round(dB, 2))
	names(sum) <- c("nesh","dB")
	return(sum)
}

generate_covariance_matrix <- function(nn,k_ave,type.network,sd.min=0.3,sd.max=1,positive.ratio=0.5){
  # @param nn number of nodes
  # @param k_ave average degree (number of edges per node)
  # @param type.network network structure
  #               random: random networks
  #                   sf: scale-free networks
  #                   sw: small-world networks
  #                   bipar: random bipartite networks
  # @param sd.min the minimum value for uniform distribution
  # @param sd.max the maximum value for uniform distribution
  # @param positive.ratio the occurence probability of positive covariance

  nl <- round(k_ave * nn / 2)
  if(type.network == "random"){
    g <- erdos.renyi.game(nn,nl,type="gnm")
  } else if(type.network == "sf"){
    g <- static.power.law.game(nn,nl,2.1,-1,loops = F,multiple = F,finite.size.correction = T)
  } else if(type.network == "sw") {
    g <- sample_smallworld(1, nn, round(k_ave / 2), 0.05, loops=F, multiple=F)
  } else if(type.network == "bipar") {
    g <- sample_bipartite(nn/2,nn/2,type="gnm",m=nl,directed=F)
  } else {
    stop("netwotk type is invalid")
  }

  #get adjacency matrix
  mtx_g <- as.matrix(get.adjacency(g))
  # get edge list
  edgelist <- get.edgelist(g)

  # generate A
  flag <- 0
  while(flag == 0){
    A <- matrix(0,nrow=nn,ncol = nn)
    for(i in 1:nl){
      if(runif(1) < positive.ratio){
        val <- runif(1,min=sd.min, max=sd.max)
      } else {
        val <- -runif(1,min=sd.min, max=sd.max)
      }
      A[edgelist[i,1],edgelist[i,2]] <- val
      A[edgelist[i,2],edgelist[i,1]] <- val
    }

    # diagonal elements
    diag(A) <- 1
    if(is.positive.definite(A)==T){
      flag <- 1
    }
    flag <- 1
  }

  return(list(mtx_g, A))
}

random.rewiring.correlation.mtx <- function(x.cor, niter, sd.min=0.3, sd.max=1, positive.ratio=0.5){
  # x.cor the original covariance matrix
  # @param niter number of edge rewiring
  # @param sd.min the minimum value for uniform distribution
  # @param sd.max the maximum value for uniform distribution
  # @param positive.ratio the occurence probability of positive covariance

	diag(x.cor) <- 0
  x.cor.original <- x.cor
	nn <- dim(x.cor)[[1]]
  x.cor.mod <- matrix(0, nn, nn)

	for(n in 1:niter){
		flag <- 0
		while(flag == 0){
			source_id <- sample(1:nn, 1)
			target_id_set <- which(abs(x.cor[source_id,]) > 0)
			if(length(target_id_set) > 0){
				flag <- 1
			}
		}
    if(length(target_id_set) == 1){
      target_id <- target_id_set[[1]]
    } else {
      target_id <- sample(target_id_set, 1)
    }
		x.cor[source_id, target_id] <- 0
		x.cor[target_id, source_id] <- 0

		flag <- 0
		while(flag == 0){
			new_target_id <- sample(1:nn, 1)
			if(x.cor.mod[source_id, new_target_id] == 0 && new_target_id != source_id && x.cor.original[source_id, new_target_id] == 0){
				flag <- 1
			}
		}

		if(runif(1) < positive.ratio){
			val <- runif(1,min=sd.min, max=sd.max)
		} else {
			val <- -runif(1,min=sd.min, max=sd.max)
		}
    x.cor.mod[source_id, new_target_id] <- val
		x.cor.mod[new_target_id, source_id] <- val
	}

  x.cor.mod <- x.cor.mod + x.cor

	net <- ifelse(abs(x.cor.mod) > 0, 1, 0)
	diag(x.cor.mod) <- 1
	return(list(net, x.cor.mod))
}
