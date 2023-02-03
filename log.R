# data from Golzadeh, M. et al. (2021) ‘A ground-truth dataset and classification model for detecting bots in GitHub issue and PR comments’, Journal of Systems and Software, 175, p. 110911. Available at: https://doi.org/10.1016/j.jss.2021.110911.

tab <- read.csv("groundtruthbots.csv");

add.href <- function(v, root="https://github.com/") {
  return(paste0('<a href=\"', root, v, '\">', v, "</a>"));
}

t2 <- data.frame(account = add.href(tab$account), project = add.href(tab$project), type=tab$type);


require(htmlTable)
cat(htmlTable(t2), file = "groundTruthBase.htm")

project.vector <- with(subset(tab, type != "Bot"), unique(project));

project.host <- sapply(strsplit(project.vector, '/'), head, 1);

fin <- read.csv("../Inscriptions I1 liste 2022-2023.csv", skip =6);

fin <- subset(fin, NOM!="");

fin$group <- sample(rep(LETTERS[1:8], 6)[1:nrow(fin)]);

declare.credits <- function(participants) {
  n <- with(participants, paste("N:", PRENOM, NOM));
  d <- paste("D: Contributor");
  d2 <- paste("D: Group", participants$group);
  
  return(paste(n, d, d2, sep="\n", collapse="\n\n"));
}


