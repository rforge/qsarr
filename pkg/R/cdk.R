cdk <- function(data = cdk, ... ){
  ###########Loading all necessary libraries###########
  library(rcdk)
  descriptors <- get.desc.categories() 
  .GlobalEnv[["descriptors"]] <- descriptors
  ###########Generating 2D CDK descriptors###########
  cdk.2d <- {
    c("org.openscience.cdk.qsar.descriptors.molecular.AutocorrelationDescriptorCharge",        
      "org.openscience.cdk.qsar.descriptors.molecular.AutocorrelationDescriptorMass",         
      "org.openscience.cdk.qsar.descriptors.molecular.AutocorrelationDescriptorPolarizability",
      "org.openscience.cdk.qsar.descriptors.molecular.CarbonTypesDescriptor",                  
      "org.openscience.cdk.qsar.descriptors.molecular.ChiChainDescriptor",                     
      "org.openscience.cdk.qsar.descriptors.molecular.ChiClusterDescriptor",                   
      "org.openscience.cdk.qsar.descriptors.molecular.ChiPathClusterDescriptor",               
      "org.openscience.cdk.qsar.descriptors.molecular.ChiPathDescriptor",                      
      "org.openscience.cdk.qsar.descriptors.molecular.EccentricConnectivityIndexDescriptor",   
      "org.openscience.cdk.qsar.descriptors.molecular.FMFDescriptor",                          
      "org.openscience.cdk.qsar.descriptors.molecular.FragmentComplexityDescriptor",           
      "org.openscience.cdk.qsar.descriptors.molecular.HybridizationRatioDescriptor",           
      "org.openscience.cdk.qsar.descriptors.molecular.KappaShapeIndicesDescriptor",            
      "org.openscience.cdk.qsar.descriptors.molecular.KierHallSmartsDescriptor",              
      "org.openscience.cdk.qsar.descriptors.molecular.MDEDescriptor",                          
      "org.openscience.cdk.qsar.descriptors.molecular.PetitjeanNumberDescriptor",             
      "org.openscience.cdk.qsar.descriptors.molecular.PetitjeanShapeIndexDescriptor",          
      "org.openscience.cdk.qsar.descriptors.molecular.TPSADescriptor",                         
      "org.openscience.cdk.qsar.descriptors.molecular.VABCDescriptor",                         
      "org.openscience.cdk.qsar.descriptors.molecular.VAdjMaDescriptor",                       
      "org.openscience.cdk.qsar.descriptors.molecular.WeightedPathDescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.WienerNumbersDescriptor",                
      "org.openscience.cdk.qsar.descriptors.molecular.ZagrebIndexDescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.BCUTDescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.ALOGPDescriptor",                
      "org.openscience.cdk.qsar.descriptors.molecular.AcidicGroupCountDescriptor",     
      "org.openscience.cdk.qsar.descriptors.molecular.AminoAcidCountDescriptor",       
      "org.openscience.cdk.qsar.descriptors.molecular.AromaticAtomsCountDescriptor",   
      "org.openscience.cdk.qsar.descriptors.molecular.AromaticBondsCountDescriptor",   
      "org.openscience.cdk.qsar.descriptors.molecular.AtomCountDescriptor",            
      "org.openscience.cdk.qsar.descriptors.molecular.BasicGroupCountDescriptor",      
      "org.openscience.cdk.qsar.descriptors.molecular.BondCountDescriptor",            
      "org.openscience.cdk.qsar.descriptors.molecular.LargestChainDescriptor",         
      "org.openscience.cdk.qsar.descriptors.molecular.LargestPiSystemDescriptor",      
      "org.openscience.cdk.qsar.descriptors.molecular.LongestAliphaticChainDescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.MannholdLogPDescriptor",         
      "org.openscience.cdk.qsar.descriptors.molecular.RotatableBondsCountDescriptor",  
      "org.openscience.cdk.qsar.descriptors.molecular.RuleOfFiveDescriptor",           
      "org.openscience.cdk.qsar.descriptors.molecular.WeightDescriptor",               
      "org.openscience.cdk.qsar.descriptors.molecular.XLogPDescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.AminoAcidCountDescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.APolDescriptor",              
      "org.openscience.cdk.qsar.descriptors.molecular.CPSADescriptor",              
      "org.openscience.cdk.qsar.descriptors.molecular.HBondAcceptorCountDescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.HBondDonorCountDescriptor",   
      "org.openscience.cdk.qsar.descriptors.molecular.TPSADescriptor")}
  .GlobalEnv[["cdk.2d"]] <- cdk.2d
  ###########Generating 3D CDK descriptors###########
  cdk.3d <- {
    c("org.openscience.cdk.qsar.descriptors.molecular.CPSADescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.GravitationalIndexDescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.LengthOverBreadthDescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.MomentOfInertiaDescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.PetitjeanShapeIndexDescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.WHIMDescriptor")
  }
  .GlobalEnv[["cdk.3d"]] <- cdk.3d
  ###########Generating CDK electronic descriptors###########
  cdk.electronic <- get.desc.names(descriptors[1])
  .GlobalEnv[["cdk.electronic"]] <- cdk.electronic
  ###########Generating CDK protein descriptors###########
  cdk.protein <- get.desc.names(descriptors[2])
  .GlobalEnv[["cdk.protein"]] <- cdk.protein 
  ###########Generating CDK topological descriptors###########
  cdk.topological <- get.desc.names(descriptors[3])
  .GlobalEnv[["cdk.topological"]] <- cdk.topological
  ###########Generating CDK geometrical descriptors###########
  cdk.geometrical <- get.desc.names(descriptors[4])
  .GlobalEnv[["cdk.geometrical"]] <- cdk.geometrical
  ###########Generating CDK constitutional descriptors###########
  cdk.constitutional <- get.desc.names(descriptors[5])
  .GlobalEnv[["cdk.constitutional"]] <- cdk.constitutional
  ###########Generating CDK hybrid descriptors###########
  cdk.hybrid <- get.desc.names(descriptors[6])
  .GlobalEnv[["cdk.hybrid "]] <- cdk.hybrid 
} 