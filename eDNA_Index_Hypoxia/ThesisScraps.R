geom_vline(xintercept = 0.66, linetype = 2, color = "red") + # most copepods die
            geom_vline(xintercept = 0.9, linetype = 2, color = "orange") + # ~50% of copepods die
            geom_vline(xintercept = 2.66, linetype = 2, color = "forestgreen") + # many copepods
  
  # Plot the GAM + save it
  # I don't think I can make geom_smooth do a beta regression -_-
  #print(ggplot(dfsplit[[i]], aes(x = DO, y = eDNA_index)) + # plot this species
  #  geom_point(color = "orange2", alpha = 0.7) +
  #  geom_smooth(method = 'gam', se = F, color = "cornflowerblue") + # visualize GAM
  #  geom_vline(xintercept = 0.66, linetype = 2, color = "red") + # most copepods die
  #  geom_vline(xintercept = 0.9, linetype = 2, color = "orange") + # ~50% of copepods die
  #  geom_vline(xintercept = 2.66, linetype = 2, color = "forestgreen") + # many copepods experience sublethal effects (e.g. less egg production)
  #  ggtitle(title) +
  #  theme_bw())
#ggsave(filename = paste(species, sep = "_", "eDNA_DO_GAM_Prelim.png"), path = here("eDNA_Index_Hypoxia", "Plots", "GAM_Initial"), width = 2500, height = 2000, units = "px")