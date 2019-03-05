# Init function

installRequired = function(x)
{
  for( i in x )
  {
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) )
    {
      #  If package was not able to be loaded then re-install
      suppressMessages(suppressWarnings(install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE );
    }
  }
}

installRequired(c("devtools","reshape2","ggplot","ggthemes","DEoptim"));


