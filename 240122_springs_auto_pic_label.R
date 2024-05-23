## Automatically labeling and exporting Springs Photos
## Adam Pingatore
## January 22, 2024


# run this line if you need any of the below packages. Some of them require you to
# be off-network to install for some reason
# install.packages("jpeg", "grid", "gridExtra", "ggplot2", "cowplot", "qpdf")

require(jpeg)
require(grid)
require(gridExtra)
require(ggplot2)
require(cowplot)
require(qpdf)

## set working directory
setwd("C:/Users/XXXXXXXXXX/Auto_Name_Springs/raw")

## point this folder to where the raw meta-processes photos are stored
fold <- "C:/Users/XXXXXXXXXX/Auto_Name_Springs/meta_resized"


## retrieve file names
filenames <- list.files(path = fold,
                        pattern = "*.JPG") 

## site code - common name association table
spring_names <- read.csv("C:/Users/XXXXXXXXXX/Auto_Name_Springs/springnames.csv")



## split up meta named files to extract info
site_list <- vector()
loc_list <- vector()
pic_list <- vector()

for(i in 1:length(filenames)){
  
  a <- strsplit(x=filenames[i], split="_")
  
  site_list[i] <- paste0(a[[1]][2], "_", a[[1]][3])
  
  loc_list[i] <- a[[1]][4]
  
  pic_list[i] <- filenames[i]
  
}

## bind lists into a df
meta <- cbind.data.frame(site=site_list, loc=loc_list, pic=pic_list)



## Start the main legwork --> 3 nested for-loops


for(i in 1:length(unique(meta$site))){
  
  ## get the index of the current site
  rr <- unique(meta$site)[i]
  
  ## get all meta data for that site
  tt <- meta[which(meta$site == rr),]
  
  
  pic_set <- list()
  for(j in 1:dim(tt)[1]){
    
    ## read in all photos for one site
    pic_set <- append(pic_set,
                      readJPEG(paste0(fold,"/",
                                      tt$pic[j])))
    
  }
  
  
  ## get number of pictures and determine how many iterations are needed in next loop
  numpic <- dim(tt)[1]
  
  size <- numpic/4
  numloop <- if(size == floor(size)){
    size
  } else{
    floor(size)+1
  }
  
  ## extract pictures. This is a series of if-statements which 
  ## all do the same thing a different number of times. Because
  ## sometimes the number of pictures is not divisible by 4 evenly.
  ## The math after the if-statements is the best way I thought of to iterate
  ## properly.
  name_list <- list()
  for(q in 1:numloop){
    
    if(numpic-(q-1)*4>=1){
      
      pic1 <- readJPEG(paste0(fold,"/",
                              paste0(tt$pic[((q-1)*4)+1])))
      
    }
    if(numpic-(q-1)*4>=2){
      
      pic2 <- readJPEG(paste0(fold,"/",
                              paste0(tt$pic[((q-1)*4)+2])))
    }
    if(numpic-(q-1)*4>=3){
      pic3 <- readJPEG(paste0(fold,"/",
                              paste0(tt$pic[((q-1)*4)+3])))
      
    }
    if(numpic-(q-1)*4>=4){
      
      pic4 <- readJPEG(paste0(fold,"/",
                              paste0(tt$pic[((q-1)*4)+4])))
      
    }
    
    
    ## set the title for the pdf
    title <- ggdraw() +
      draw_label(
        paste0(spring_names$name[which(spring_names$spring == tt$site[q])],
               " (",tt$site[q],")"),
        fontface = 'bold',
        x = 0,
        y=0,
        hjust = -0.1,
        vjust=-3
      )
    
    
    ## layout the photos in a grid on a 2x2 grid and label them.
    ## Again, multiple if-statements to capture instances where
    ## n photo is not divisible by 4 evenly.
    if(numpic-(q-1)*4>=1){
      pl <- list(rasterGrob(pic1))
      
      a<-plot_grid(pl[[1]], scale=0.9,
                   labels = c(tt$loc[((q-1)*4)+1],
                              tt$loc[((q-1)*4)+2],
                              tt$loc[((q-1)*4)+3],
                              tt$loc[((q-1)*4)+4]),
                   label_x=.4,label_y=1.03, ncol=2, nrow=2)
    }
    if(numpic-(q-1)*4>=2){
      pl <- list(rasterGrob(pic1),
                 rasterGrob(pic2))
      
      a<-plot_grid(pl[[1]],pl[[2]], scale=0.9,
                   labels = c(tt$loc[((q-1)*4)+1],
                              tt$loc[((q-1)*4)+2],
                              tt$loc[((q-1)*4)+3],
                              tt$loc[((q-1)*4)+4]),
                   label_x=.4,label_y=1.03, ncol=2, nrow=2)
    }
    if(numpic-(q-1)*4>=3){
      pl <- list(rasterGrob(pic1),
                 rasterGrob(pic2),
                 rasterGrob(pic3))
      
      a<-plot_grid(pl[[1]],pl[[2]],pl[[3]], scale=0.9,
                   labels = c(tt$loc[((q-1)*4)+1],
                              tt$loc[((q-1)*4)+2],
                              tt$loc[((q-1)*4)+3],
                              tt$loc[((q-1)*4)+4]),
                   label_x=.4,label_y=1.03, ncol=2, nrow=2)
    }
    if(numpic-(q-1)*4>=4){
      pl <- list(rasterGrob(pic1),
                 rasterGrob(pic2),
                 rasterGrob(pic3),
                 rasterGrob(pic4))
      
      a<-plot_grid(pl[[1]],pl[[2]],pl[[3]],pl[[4]], scale=0.9,
                   labels = c(tt$loc[((q-1)*4)+1],
                              tt$loc[((q-1)*4)+2],
                              tt$loc[((q-1)*4)+3],
                              tt$loc[((q-1)*4)+4]),
                   label_x=.4,label_y=1.03, ncol=2, nrow=2)
      
      
    }
    
    ## save plot grid to a new plot grid to add the title properly
    b<-plot_grid(title, a, ncol=1, rel_heights = c(0.1, 1))
    
    ## save the "raw" pdfs. Note, in these saves a site is split up into multiple
    ## pdfs, we'll combine them later
    ggsave(paste0(rr,"_WY23_pics_", q, ".pdf"),b,width=11,height=8.5, units="in")  
    
    ## save a list of all the file names which belong to one site
    name_list <- append(name_list, paste0(rr,"_WY23_pics_", q, ".pdf"))
  }
  
  ## use the list of file names to concatenate the pdfs and save them to a 
  ## final "combine" folder 
  pdf_combine(input = name_list,
              output=paste0("C:/Users/XXXXXXXXXX/Auto_Name_Springs/combine/",
                            rr,"_WY23_pics.pdf"))
  
  

}


## END

