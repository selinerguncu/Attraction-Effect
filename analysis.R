modelSales <- lmer ( z_sales ~ lag_z_sales
                 
                 + z_price             + z_adv            + z_promo            + z_dist            + z_cons            + z_liking
                 + comp_z_price        + comp_z_adv       + comp_z_promo       + comp_z_dist       + comp_z_cons       + comp_z_liking 
                 + decoy_z_price       + decoy_z_adv      + decoy_z_promo      + decoy_z_dist      + decoy_z_cons      + decoy_z_liking 
                 + lag_z_price         + lag_z_adv        + lag_z_promo        + lag_z_dist        + lag_z_cons        + lag_z_liking
                 + comp_lag_z_price    + comp_lag_z_adv   + comp_lag_z_promo   + comp_lag_z_dist   + comp_lag_z_cons   + comp_lag_z_liking 
                 + decoy_lag_z_price   + decoy_lag_z_adv  + decoy_lag_z_promo  + decoy_lag_z_dist  + decoy_lag_z_cons  + decoy_lag_z_liking
                 
                 + z_dominance*z_adv + z_dominance*z_price + z_dominance*z_promo + time + (1 | pair:brandsetId) + (-1 + z_dominance | pair), data = mergedBrandsets)


# modelSalesAlt <- lmer ( z_sales ~ lag_z_sales
                 
#                  + z_price             + z_adv            + z_promo            + z_dist            + z_cons            + z_liking
#                  + comp_z_price        + comp_z_adv       + comp_z_promo       + comp_z_dist       + comp_z_cons       + comp_z_liking 
#                  + decoy_z_price       + decoy_z_adv      + decoy_z_promo      + decoy_z_dist      + decoy_z_cons      + decoy_z_liking 
#                  + lag_z_price         + lag_z_adv        + lag_z_promo        + lag_z_dist        + lag_z_cons        + lag_z_liking
#                  + comp_lag_z_price    + comp_lag_z_adv   + comp_lag_z_promo   + comp_lag_z_dist   + comp_lag_z_cons   + comp_lag_z_liking 
#                  + decoy_lag_z_price   + decoy_lag_z_adv  + decoy_lag_z_promo  + decoy_lag_z_dist  + decoy_lag_z_cons  + decoy_lag_z_liking
                 
#                 + time + (1 | pair:brandsetId) + (-1 + z_dominance + z_dominance*z_adv + z_dominance*z_price + z_dominance*z_promo | pair), data = mergedBrandsets)


modelCons <- lmer ( z_cons ~ lag_z_cons
                 
                 + z_price             + z_adv            + z_promo            + z_dist            + z_liking
                 + comp_z_price        + comp_z_adv       + comp_z_promo       + comp_z_dist       + comp_z_cons       + comp_z_liking 
                 + decoy_z_price       + decoy_z_adv      + decoy_z_promo      + decoy_z_dist      + decoy_z_cons      + decoy_z_liking 
                 + lag_z_price         + lag_z_adv        + lag_z_promo        + lag_z_dist        + lag_z_liking
                 + comp_lag_z_price    + comp_lag_z_adv   + comp_lag_z_promo   + comp_lag_z_dist   + comp_lag_z_cons   + comp_lag_z_liking 
                 + decoy_lag_z_price   + decoy_lag_z_adv  + decoy_lag_z_promo  + decoy_lag_z_dist  + decoy_lag_z_cons  + decoy_lag_z_liking
                 
                 + z_dominance*z_adv + z_dominance*z_price + z_dominance*z_promo + time + (1 | pair:brandsetId) + (-1 + z_dominance | pair), data = mergedBrandsets)


modelLiking <- lmer ( z_liking ~ lag_z_liking
                 
                 + z_price             + z_adv            + z_promo            + z_dist            + z_cons
                 + comp_z_price        + comp_z_adv       + comp_z_promo       + comp_z_dist       + comp_z_cons       + comp_z_liking 
                 + decoy_z_price       + decoy_z_adv      + decoy_z_promo      + decoy_z_dist      + decoy_z_cons      + decoy_z_liking 
                 + lag_z_price         + lag_z_adv        + lag_z_promo        + lag_z_dist        + lag_z_cons
                 + comp_lag_z_price    + comp_lag_z_adv   + comp_lag_z_promo   + comp_lag_z_dist   + comp_lag_z_cons   + comp_lag_z_liking 
                 + decoy_lag_z_price   + decoy_lag_z_adv  + decoy_lag_z_promo  + decoy_lag_z_dist  + decoy_lag_z_cons  + decoy_lag_z_liking
                 
                 + z_dominance*z_adv + z_dominance*z_price + z_dominance*z_promo + time + (1 | pair:brandsetId) + (-1 + z_dominance | pair), data = mergedBrandsets)


modelCompSales <- lmer ( comp_z_sales ~ comp_lag_z_sales
                 
                 + z_price             + z_adv            + z_promo            + z_dist            + z_cons            + z_liking
                 + comp_z_price        + comp_z_adv       + comp_z_promo       + comp_z_dist       + comp_z_cons       + comp_z_liking 
                 + decoy_z_price       + decoy_z_adv      + decoy_z_promo      + decoy_z_dist      + decoy_z_cons      + decoy_z_liking 
                 + lag_z_price         + lag_z_adv        + lag_z_promo        + lag_z_dist        + lag_z_cons        + lag_z_liking
                 + comp_lag_z_price    + comp_lag_z_adv   + comp_lag_z_promo   + comp_lag_z_dist   + comp_lag_z_cons   + comp_lag_z_liking 
                 + decoy_lag_z_price   + decoy_lag_z_adv  + decoy_lag_z_promo  + decoy_lag_z_dist  + decoy_lag_z_cons  + decoy_lag_z_liking
                 
                 + z_dominance*z_adv + z_dominance*z_price + z_dominance*z_promo + time + (1 | pair:brandsetId) + (-1 + z_dominance | pair), data = mergedBrandsets)
