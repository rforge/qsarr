cdk <- function(data = cdk, ... ){
  ###########Loading all necessary libraries###########
  library(rcdk)
  descriptors.func <- get.desc.categories() 
  assign("descriptors", descriptors.func, envir=.GlobalEnv)
  ###########Generating 2D CDK descriptors###########
  cdk.2d.func <- {
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
      "org.openscience.cdk.qsar.descriptors.molecular.TPSADescriptor")
  }
  assign("cdk.2d", cdk.2d.func, envir=.GlobalEnv)
  ###########Generating 3D CDK descriptors###########
  cdk.3d.func <- {
    c("org.openscience.cdk.qsar.descriptors.molecular.CPSADescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.GravitationalIndexDescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.LengthOverBreadthDescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.MomentOfInertiaDescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.PetitjeanShapeIndexDescriptor",
      "org.openscience.cdk.qsar.descriptors.molecular.WHIMDescriptor")
  }
  assign("cdk.3d", cdk.3d.func , envir=.GlobalEnv)
  ###########Generating CDK electronic descriptors###########
  cdk.electronic.func <- get.desc.names(descriptors[1])
  assign("cdk.electronic", cdk.electronic.func, envir=.GlobalEnv)
  ###########Generating CDK protein descriptors###########
  cdk.protein.func <- get.desc.names(descriptors[2])
  assign("cdk.protein", cdk.protein.func, envir=.GlobalEnv)
  ###########Generating CDK topological descriptors###########
  cdk.topological.func <- get.desc.names(descriptors[3])
  assign("cdk.topological", cdk.topological.func, envir=.GlobalEnv)
  ###########Generating CDK geometrical descriptors###########
  cdk.geometrical.func <- get.desc.names(descriptors[4])
  assign("cdk.geometrical", cdk.geometrical.func, envir=.GlobalEnv)
  ###########Generating CDK constitutional descriptors###########
  cdk.constitutional.func <- get.desc.names(descriptors[5])
  assign("cdk.constitutional", cdk.constitutional.func, envir=.GlobalEnv)
  ###########Generating CDK hybrid descriptors###########
  cdk.hybrid.func <- get.desc.names(descriptors[6])
  assign("cdk.hybrid", cdk.hybrid.func, envir=.GlobalEnv)
} 
