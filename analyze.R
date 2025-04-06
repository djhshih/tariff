library(io)

# trade volumes are in millions of USD nominally
# seasonal adjustment is annual, so annual figures are not affected

us.trades.all <- qread("data/country.csv");
colnames(us.trades.all) <- tolower(colnames(us.trades.all));

regions <- unique(us.trades.all$ctyname);
names(regions) <- regions;

regions <- setdiff(regions,
	c(
		"World, Not Seasonally Adjusted",
		"World, Seasonally Adjusted",
		"Asia",
		"Pacific Rim",
		"Advanced Technology Products",
		"Europe",
		"USMCA with Mexico (Consump)",
		"USMCA with Canada (Consump)",
		"North America"
	)
);

exempt.regions <- c("Canada", "Mexico", "Russia", "Korea, North", "Cuba", "Belarus");

# calculate the tariff hike based on a specific elasticity and passthrough
get_tariff <- function(import_change, import, elasticity, passthrough, discount=0, base=0) {
	y <- pmax(base,
		ifelse(import > 0,
			(1 - discount) * import_change / (elasticity * passthrough * import),
			0
		)
	);
	names(y) <- names(import_change);
	y
}

get_import_change <- function(tariff_change, import, elasticity, passthrough) {
	tariff_change * elasticity * passthrough * import
}

# generate matrix of import and export volumes for each region,
# with regions along the rows
us.trades <- lapply(regions,
	function(region) {
		idx <- which(us.trades.all$year == 2024 & us.trades.all$ctyname == region);
		if (length(idx) > 0) {
			c(us.trades.all$iyr[idx], us.trades.all$eyr[idx])
		} else {
			c(0, 0)
		}
	}
);
us.trades <- matrix(unlist(us.trades), byrow=TRUE, ncol=2);
rownames(us.trades) <- regions;
colnames(us.trades) <- c("import", "export");

zero.idx <- which(rowSums(us.trades) == 0);

# remove regions with no trades
us.trades <- us.trades[-zero.idx, ];
regions <- regions[-zero.idx];


# trade balance: export - import
us.balances <- apply(us.trades, 1, diff);

# proposed tariff change
tariff.changes <- get_tariff(us.balances, us.trades[,1],
	discount=0.5, elasticity=-4, passthrough=0.25, base=0.1);

tariff.changes[exempt.regions] <- 0;
sort(tariff.changes)

# use correct formula for effect on import 
us.import.changes <- get_import_change(tariff.changes2, us.trades[,1],
	elasticity=-3.937, passthrough=0.945);

sort(us.import.changes, decreasing=TRUE)

us.trades1 <- us.trades + cbind(us.import.changes, 0);
us.balances1 <- apply(us.trades1, 1, diff);
summary(us.balances1)
sum(us.balances1)
sum(us.balances1) / sum(us.balances) - 1

# direct effect on EU's GDP, which is estimated to be 20.29 T in 2025
eu.gdp.2025 <- 20.29e12;
eu.gdp.2025.adjusted <- eu.gdp.2025 + us.import.changes["European Union"]*1e6;
eu.gdp.2025.adjusted / eu.gdp.2025 - 1

# direct effect on China's GDP, which is 17794.78 B USD in 2023
# GDP growth in 2024 is expected to be 5%
china.gdp.2024 <- 17794.78e9 * 1.05;
china.gdp.2025 <- china.gdp.2024 * 1.05;
china.gdp.2025.adjusted <- china.gdp.2025 + us.import.changes["China"]*1e6;
# percentage effect on China's GDP vs. projected 2025 growth target of 5%
china.gdp.2025.adjusted / china.gdp.2025 - 1

# direct effect on world GDP, which is 106.2 T in 2023
# GDP growth in 2024 is expected to be 2.8%
world.gdp.2024 <- 106.2e12 * 1.028;
world.gdp.2025 <- world.gdp.2024 * 1.028;
world.export.reduction <- sum(pmin(0, us.import.changes))*1e6;
world.gdp.2025.adjusted <- world.gdp.2025 + world.export.reduction;
world.gdp.2025.adjusted / world.gdp.2025 - 1

us.gdp.2024 <- 27.72e12 * 1.029;
us.gdp.2025 <- us.gdp.2024 * 1.029;


# ---

# use correct parameter values
# elasticity is w.r.t. the import price
# passthrough should be w.r.t. the import price as well

# passthrough = 18.9 / 20 = 0.945 from Cavallo 2021
# elasticity = 3.937 from Soderbery 2018

tariff.changes2 <- get_tariff(us.balances, us.trades[,1],
	discount=0, elasticity=-3.937, passthrough=0.945, base=0);

tariff.changes2[exempt.regions] <- 0;
sort(tariff.changes2)

import.changes2 <- get_import_change(tariff.changes2, us.trades[,1],
	elasticity=-3.937, passthrough=0.945);

sort(import.changes2, decreasing=TRUE)

us.trades2 <- us.trades + cbind(import.changes2, 0);
us.balances2 <- apply(us.trades2, 1, diff);
sum(us.balances2)
sum(us.balances2) / sum(us.balances) - 1

# ---

hist(log(us.trades[, 1]), breaks=20)
hist(log(us.trades[, 2]), breaks=20)

# ---

# generate full import matrix for each region
J <- length(regions) + 1;

init_trades <- function(us.trades, seed=1234) {
	J <- nrow(us.trades) + 1;

	# X[i, j] is trade volume from region i to j
	X <- matrix(0, nrow=J, ncol=J);
	rownames(X) <- colnames(X) <- c("United States", regions);

	# fill in matrix with trade data from US (FIXME)
	set.seed(seed)
	X[1:J, 1:J] <- sample(us.trades, J*J, replace=TRUE);

	# fill in US data
	X[-1, 1] <- us.trades[, 1];
	X[1, -1] <- us.trades[, 2];

	# ensure that diagonal is 0 (international trades are between countries)
	diag(X) <- 0;

	X;
}

# ---

# Scenario: each country implement tariff based on trade deficit

X <- init_trades(us.trades);

# total exports - total imports for US
sum(us.balances)
sum(X[1, ]) - sum(X[, 1])

sum(diag(X))

# balanced aggregate trade condition
# sum(X[1, ]) - sum(X[, 1]) == 0

agg.balances0 <- unlist(lapply(1:J, function(j) sum(X[j, ]) - sum(X[, j])));
mean(agg.balances0)
sd(agg.balances0)
summary(agg.balances0)

X0 <- X;

elasticity <- -3.937;
passthrough <- 0.945;

for (j in 1:J) {
	message(j)

	balances <- X[j, ] - X[, j];

	tariff.changes <- get_tariff(balances, X[, j],
		elasticity=elasticity, passthrough=passthrough, base=0);

	import.changes <- get_import_change(tariff.changes, X[, j],
		elasticity=elasticity, passthrough=passthrough);

	X[, j] <- X[, j] + import.changes;
}

agg.balances <- unlist(lapply(1:J, function(j) sum(X[j, ]) - sum(X[, j])));
summary(agg.balances)

agg.balances0[1]
agg.balances[1]

# US trade balance before and after
sum(X0[1, ] - X0[, 1])
sum(X[1, ] - X[, 1])

sum(diag(X))

# decrease in import for the US
us.import.change <- sum(X[, 1]) - sum(X0[, 1]);
us.import.change / sum(X0[, 1])

# decrease in export for the US
us.export.change <- sum(X[1, ]) - sum(X0[1, ]);
us.export.change / sum(X0[1, ])

us.gdp.2025.adjusted2 <- us.gdp.2025 + us.export.change * 1e6;
us.gdp.2025.adjusted2 / us.gdp.2025 - 1

# this estimate is likely not very accurate
world.export.change <- sum(X - X0);
world.export.change / sum(X0)

# NB we did not start with real world volume data, so
#    we cannot look at impact on GDP

# ---

# Scenario: each country implement reciprical tariff against US

X <- init_trades(us.trades);
X0 <- X;

agg.balances0 <- unlist(lapply(1:J, function(j) sum(X[j, ]) - sum(X[, j])));
summary(agg.balances0)

elasticity <- -3.937;
passthrough <- 0.945;

for (r in 1:3) {
	j <- 1;
	balances <- X[j, ] - X[, j];

	tariff.changes <- get_tariff(balances, X[, j],
		elasticity=elasticity, passthrough=passthrough, base=0);

	import.changes <- get_import_change(tariff.changes, X[, j],
		elasticity=elasticity, passthrough=passthrough);

	X[, j] <- X[, j] + import.changes;

	for (j in 2:J) {
		reciprocal <- tariff.changes[j];
		import.change <- get_import_change(reciprocal, X[1, j],
			elasticity=elasticity, passthrough=passthrough);
		X[1, j] <- X[1, j] + import.change;
	}
}

X[1, ]

agg.balances <- unlist(lapply(1:J, function(j) sum(X[j, ]) - sum(X[, j])));

# US trade balance before and after
agg.balances0[1]
agg.balances[1]
agg.balances[1] / agg.balances0[1] - 1

# decrease in import for the US
us.import.change <- sum(X[, 1]) - sum(X0[, 1]);
us.import.change / sum(X0[, 1])

# decrease in export for the US
us.export.change <- sum(X[1, ]) - sum(X0[1, ]);
us.export.change / sum(X0[1, ])

# US goods export as a fraction of GDP
sum(X0[1, ]) * 1e6 / us.gdp.2025

us.gdp.2025.adjusted2 <- us.gdp.2025 + us.export.change * 1e6;
us.gdp.2025.adjusted2 / us.gdp.2025 - 1

# this estimate is likely not very accurate
world.export.change <- sum(X - X0);
world.export.change / sum(X0)

# ---

# Scenario: US implement tariff and other countries do nothing

X <- init_trades(us.trades);
X0 <- X;

agg.balances0 <- unlist(lapply(1:J, function(j) sum(X[j, ]) - sum(X[, j])));
summary(agg.balances0)

elasticity <- -3.937;
passthrough <- 0.945;

j <- 1;
balances <- X[j, ] - X[, j];

tariff.changes <- get_tariff(balances, X[, j],
	elasticity=elasticity, passthrough=passthrough, base=0);

import.changes <- get_import_change(tariff.changes, X[, j],
	elasticity=elasticity, passthrough=passthrough);

X[, j] <- X[, j] + import.changes;


agg.balances <- unlist(lapply(1:J, function(j) sum(X[j, ]) - sum(X[, j])));

# US trade balance before and after
agg.balances0[1]
agg.balances[1]
agg.balances[1] / agg.balances0[1] - 1

# decrease in import for the US
us.import.change <- sum(X[, 1]) - sum(X0[, 1]);
us.import.change / sum(X0[, 1])

# decrease in export for the US
us.export.change <- sum(X[1, ]) - sum(X0[1, ]);
us.export.change / sum(X0[1, ])

# US goods export as a fraction of GDP
sum(X0[1, ]) * 1e6 / us.gdp.2025

us.gdp.2025.adjusted2 <- us.gdp.2025 + us.export.change * 1e6;
us.gdp.2025.adjusted2 / us.gdp.2025 - 1

# this estimate is likely not very accurate
world.export.change <- sum(X - X0);
world.export.change / sum(X0)

# ---

