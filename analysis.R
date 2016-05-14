#Standardizing variables by subtracting the mean and dividing by 2 standard deviations

# carry (t-1) next to (t)


sales ~ log(price) + adv + promo + dist + cons + liking
+ log(comp_price) + comp_adv + comp_promo + comp_dist + comp_cons + comp_liking 
+ log(decoy_price) + decoy_adv + decoy_promo + decoy_dist + decoy_cons + decoy_liking 
+ log(decoy_price) + otherAdv + otherPromo + otherDist + otherCons + otherLiking 
+ dominance + dominance*adv + + dominance*log(price) + dominance*promo



regression <- lmer (log(sales) ~ log(price) + adv + promo + dist + cons + liking
                 + log(comp_price) + comp_adv + comp_promo + comp_dist + comp_cons + comp_liking 
                 + log(decoy_price) + decoy_adv + decoy_promo + decoy_dist + decoy_cons + decoy_liking 
                 + log(decoy_price) + otherAdv + otherPromo + otherDist + otherCons + otherLiking 
                 + dominance + dominance*adv + + dominance*log(price) + dominance*promo + time + (1 | pair), data = mergedBrandsets)

summary(regression)

