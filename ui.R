library(shiny)

# source("package_handling.R") 
# libraries <- c("shinyFiles")
# print(libraries)
# for(library in libraries) 
# { 
#   print("lib server")
#   print(library)
#   if(!UsePackage(library))
#   {
#     stop("Error!", library)
#   }
# }

#icon=img(src = "outline3.png", height = 72, width = 72),
#title=div(img(src="RGB_overlay2.png",height=30,width=60)), 
shinyUI(

  
  navbarPage( 
                    title= "SquasshAnalyst",
                    #title=div(img(src="icon7.png",height=48,width=52,align="top"),'Squassh Analyst'),
                    id = "tabs",
                   
                   tabPanel(title ="Data input", 
                            sidebarLayout
                            (
                              mainPanel(
                                #img(src="RGB_overlay2.png", height = 400, width = 400),
                            h4("Select Squassh data folder"),
                            verbatimTextOutput("path"),
                            #uiOutput("seldir"), 

                            #conditionalPanel(
                              #condition= "output.platform == 'TRUE'",
                            fluidRow(  
                              column( 3, actionButton("selectdirectory", label = "Select Directory")
                              ),
                              column(6,
                                     conditionalPanel(
                                       condition= "output.platform != 'TRUE'",
                                     h5(textOutput("warningw"))       
                                     )
                                     )
                         
                            ),
                            #),
                            br(),
                              textOutput("nbconds"),
                              verbatimTextOutput("condnames"),
                     


                            
                
                            #br(),
                            #h4('Indicate if data is from live cell imaging :'),
                            #checkboxInput("ismovie","live cell imaging", value=FALSE),
                            #conditionalPanel(
                            #  condition = "input.ismovie == true",
                            #  p('Live cell')
                            #),
                            conditionalPanel(
                             condition = "false",
                             textOutput("platform"),
                             textOutput("movie")
                            ),
                            br(),
                            uiOutput("optional"),
                            uiOutput("ch1name"),
                            uiOutput("ch2name"),
                            uiOutput("ch3name")
                            ),

                            
                            sidebarPanel(
                              h4("Welcome to SquasshAnalyst!"),
                              div(img(src="icon.png", height = 150, width = 150), style="text-align: center;"),
                              #img(src="icon2.png", height = 150, width = 150, "Welcome to SquasshAnalyst!"),
                              p(align='justify','SquasshAnalyst is a data analysis software designed to be used after the subcellular image segmentation tool Squassh
                                to quantify subcellular objects features, generate charts and perform statistical analysis.'),
                              h5("Data input"),
                              p(align='justify',"Select a folder containing Squassh analyzed fluorescence microscopy images."),
                              h5("Segmentation overview"),
                            p(align='justify',"Survey Squassh segmentation and exclude images from analysis"),
                            h5("Analysis"),
                            p(align='justify',"Compute image features and compare across conditions"),
                            #h5("Live cell analysis"),
                            #p("Display evolution over time of live cells features")
                            
                            br(),                            
                            p("Created by Aurélien Rizk & Philipp Berger"),
                            p("Contact :", span(a('Philipp.Berger@psi.ch', href='mailto:philipp.berger@psi.ch?subject=Squassh Analyst feedback'))),
                            p('Paul Scherrer Institute, Switzerland')
                            
                            
                            
                            ),
                            position=c("right"),
                            
                            )
                            
                            
                            ),
                   
                   
                   tabPanel("Segmentation overview",
                            #select condition to display
                            selectizeInput("contact_sheet_condition", label = h4("Select previewed condition"), 
                                        choices=c("C1" , "C2", "C3" ), width='604px', 
                                        selected = 1),
                            conditionalPanel(
                              condition = "input.contact_sheet_condition != 0",
                            p("Squassh segmentation parameters :"),
                            verbatimTextOutput("seg_params")
                            ),
                            wellPanel(
                              h3(textOutput("images_caption")),
                              # This outputs the dynamic UI component
                              uiOutput("ui")
                            )
                            

                   ),
                   tabPanel("Analysis",
                            
                            fluidPage
                            (
                             
                              wellPanel(
                                h4("Image attribute and data selection"),
                                fluidRow(
                                  column(4,
                                selectInput("feature_choice", label = "Subcellular object / image attribute", 
                                            c("Mean object volume", 
                                              "Mean object length", 
                                              "Mean object intensity",
                                              "Colocalization (number)", 
                                              "Colocalization (size)", 
                                              "Colocalization (signal)",
                                              "Pearson correlation", 
                                              "Pearson correlation inside cell mask", 
                                              "Total object volume Venn diagram",
                                              "Total object signal",
                                              "Total object signal / Cell Size",
                                              "Total object volume",
                                              "Total object volume / Cell Size",
                                              "Object number",
                                              "Object number / Cell Size"
                                            ), 
                                            width='605px',
                                            selected = 1)
                                        #),
                                  ),
                                 column(8,
                                  selectInput('condselect',
                                              'Compare experimental conditions :',
                                              choices=c("1","2"),
                                              multiple=TRUE,
                                              selectize=TRUE,
                                              width='100%' ),
                                  actionButton('applychanges', label="Apply conditions changes"),
                                  actionButton('selectall', label="Select all"),
                                  actionButton('deselectall', label="Deselect all")
                                 )
                                ),

                                
                                fluidRow(

                                #pearson correlation
                                  column(4,
                                conditionalPanel(
                                  
                                  condition = " output.nbconds.lastIndexOf('3') > 10 && (input.feature_choice == 'Pearson correlation' ||
                                  input.feature_choice == 'Pearson correlation inside cell mask')
                                  ",
                                  
                                  selectizeInput("channel_choice_pearson", label=("Select two channels for Pearson correlation :"), width='601px',
                                                 choices = list("Channel 1" = 1, "Channel 2" = 2, "Channel 3" = 3), 
                                                 selected= c(1,2), multiple=TRUE, options = list(maxItems = 2))
                                  
                                ),
                                #all other features
                                conditionalPanel(
                                  
                                  condition = "input.feature_choice != 'Pearson correlation' &&
                                  input.feature_choice != 'Pearson correlation inside cell mask' &&
                                  input.feature_choice != 'Colocalization (number)' &&
                                  input.feature_choice != 'Colocalization (size)' &&
                                  input.feature_choice != 'Colocalization (signal)' &&
                                  input.feature_choice != 'Total object volume Venn diagram'
                                  ",
                                  br(),
                                  
                                  selectInput("channel_choice_all", label=("Feature computed on channel :"), width='500px',
                                              choices = list("Channel 1" = 1, "Channel 2" = 2, "Channel 3" = 3) 
                                  )
                                  
                                  
                                ),
                                #colocalization 
                                conditionalPanel(
                                  
                                  condition = "input.feature_choice == 'Colocalization (number)'
                                  || input.feature_choice == 'Colocalization (size)'
                                  || input.feature_choice == 'Colocalization (signal)'",
                                  
                                  selectInput("channel_choice_coloc", label=("Percentage of channel ..."), width='602px',
                                              choices = list("Channel 1" = 1, "Channel 2" = 2, "Channel 3" = 3), 
                                              selected = 1),
                                  
                                  selectizeInput("channel_choice_coloc2", label=("... colocalizing with channel(s) :"), width='603px',
                                                 choices = list("Channel 1" = 1, "Channel 2" = 2, "Channel 3" = 3),
                                                 multiple=TRUE,
                                                 options = list(maxItems = 2)
                                  
                                      )
                                    ),
                                
                                conditionalPanel(
                                  condition=("input.feature_choice == 'Total object volume Venn diagram'"),
                                  p('Venn diagram is computed on first selected condition.')
                                )
                                
                                  ),
                                
                                column(8,
                                       br(),
                                p("Images excluded from analysis :"),
                                verbatimTextOutput("excluded_imgs")
                                )
                                )
                                
                                
#                                 fluidRow(
#                                   
#                                   column(4,
#                                          h4("Chart type"),
#                                          selectInput("graph_choice",label="",
#                                                      choices = list("Box plot", "Strip chart", "Bar chart"),
#                                                      selected = 1)
#                                   ),
#                                   column(4,
#                                          br(),br(),
#                                          #h4("Select image feature"),
#                                          #actionButton("pdf_export", "Export as pdf"),
#                                          downloadButton('exportPlot', 'Export pdf')
#                                   )
#                                 )
                                
                              ),

                              wellPanel(
                                h4("Chart type"),
                                fluidRow(
                                  
                                  column(4,
                                         #h4("Chart type"),
                                         conditionalPanel(
                                          condition=("input.feature_choice != 'Total object volume Venn diagram'
                                                       && output.movie == 'FALSE'
                                                     "),
                                         selectInput("graph_choice",label="",
                                                     choices = list("Box plot", "Strip chart", "Bar chart (Mean with SEM)"),
                                                     selected = 1)
                                         ),
                                         conditionalPanel(
                                           condition=("
                                                     output.movie == 'TRUE'
                                                      "),
                                           sliderInput("rolltmean", "Rolling mean over time", 
                                                       min=1, max=20, value=1)
                                             )
                                  ),
                                  conditionalPanel(
                                    condition=("input.feature_choice != 'Total object volume Venn diagram'
                                                       && output.movie == 'FALSE'
                                                     "),
                                    column(2,
                                           br(),
                                         checkboxInput("displaysort","Sort conditions", value=FALSE)       
                                         )
                                  ),
                                  conditionalPanel(
                                    condition=("output.movie == 'TRUE'
                                                     "),
                                    column(2,
                                           br(),
                                           actionButton("compare_movie","Keep plot for comparison"),
                                           actionButton("compare_reset","Reset comparison")
                                    )
                                  ),
                                  
                                  
                                  column(2,offset=2,
                                         #h4("Select image feature"),
                                         #actionButton("pdf_export", "Export as pdf"),
                                         downloadButton('exportPlot', 'Export pdf')

                                  ),
                                  column(2,
                                         conditionalPanel(
                                           condition=("input.feature_choice != 'Total object volume Venn diagram'
                                                      && output.movie == 'FALSE'
                                                      "),
                                           downloadButton('exportcsv', 'Export csv')
                                          )                                         
                                         )
                                  
                                ),
                                #tags$style(type='text/css', "#displaysort {margin-top: 20px;}"),
                                tags$style(type='text/css', "#exportPlot {margin-top: 20px;}"),
                                tags$style(type='text/css', "#exportcsv {margin-top: 20px;}")
                              ),
                                
                              plotOutput("boxplot",width='100%', height='550px'),
                                           

                            
                                     wellPanel(
                                     h4("Subcellular objects and image filters"),
                                     fluidRow(
                                       
                                       
#                                      column(4,
#                                             h5("Channel 1"),
#                                        sliderInput("minsizeC1", "Minimum object size [pixel^3]:", 
#                                                    min=0, max=50, value=0),
#                                        sliderInput("maxsizeC1", "Maximum object size [pixel^3]:", 
#                                                  min=100, max=10000, value=10000),
#                                        sliderInput("minintC1", "Mininmum intensity [0,1]:", 
#                                                  min=0, max=1, value=0, step=0.01),
#                                       sliderInput("minobjC1", "Mininimum #objects per image:", 
#                                                  min=0, max=20, value=0)
#                                      ),
                                     
                                     uiOutput("condch1"), 
                                     uiOutput("condch2"),  
                                     
                                     uiOutput("condch3")
                                                               
                                     )


                                     
                              ),
                        conditionalPanel(
                                condition="output.movie == 'FALSE'",
                                
                                wellPanel(  
                                  h4("Most representative image"),
                                  selectInput('repres',
                                              'Most representative image for condition :',
                                              choices=c(Choose ='','No conditions computed.'),
                                              selectize=TRUE,
                                              width='100%' ),
                                  conditionalPanel(
                                    condition="input.repres!='' && input.repres!='No conditions computed.'",
                                    
                                    textOutput('attrrep'),
                                    br(),
                                    strong(textOutput('namerep')),
                                    textOutput('ch1rep',inline=TRUE),
                                    textOutput('ch2rep',inline=TRUE),
                                    textOutput('ch3rep',inline=TRUE), 
                                    tags$style(type='text/css', '#ch1rep {color: red;}'),
                                    tags$style(type='text/css', '#ch2rep {color: green;}'),
                                    tags$style(type='text/css', '#ch3rep {color: blue;}'),
                                    fluidRow(
                                      column(6,
                                             imageOutput('imgrep', width='512px', height='256px')
                                      ),
                                      column(5,offset=1,
                                             textOutput('valuerep')
                                      )
                                    )
                                    
                                  )
                                  
                                  
                                  
                                )  
                        ),

                          
                          
                            wellPanel(
                            h4("Statistical analysis"),
                            selectInput('statmethod',
                                        'Method choice :',
                                        choices=list(
                                          'One way ANOVA, multiple comparisons with Tukey HSD (assume Gaussian distribution)'=1,
                                          'Kruskal-Wallis, multiple comparisons with Dunn\'s test, Bonferroni adjusted (non parametric, do not assume Gaussian distribution)'=2
                                          ),
                                        width='100%',
                                        #width='600px',
                                        selected = 1),
                            #actionButton("stat_display", "Compute one way ANOVA and Tukey analysis."),
                             #conditionalPanel(
                               #condition = "input.stat_display != 0",
                               verbatimTextOutput("stats_disp"))
                            )
                             #)
                            
                            
                            
                   )
#                    tabPanel("Venn Diagram",
#                             
#                             
#                             sidebarLayout
#                             (
#                               
#                               sidebarPanel("sidebar"),
#                               
#                               mainPanel("main plot area")
#                             )
#                    ),
                   ,
                   tabPanel("Help",
                            p(align='justify','Squassh Analyst is used after Squassh to survey Squassh segementation, 
                              compute subcellular objects features, generate charts and perform statistical analysis.'),
                            
                            h3('Data input'),
                            
                            p(align='justify',"Select a folder that contains Squassh analyzed fluorescence microscopy images."),
                            p(align='justify',span(strong("Directory selection:")),'
                                When selecting a directory Squassh Analyst will recursively search 
                                for all \'.csv\' data files generated by Squassh 
                              in subdirectories and will display found conditions data.
                              Each subdirectory should contain files corresponding to at most
                              one conditon (contact sheet image files and \'.csv\' files). 
                              All conditions should contain the same type of data (2D/3D, one channel/two channels/three channels, fixed images/movie).
                              When using Squassh Analyst on Windows computer the directory selection window opens in R.'),                          
                            p(align='justify',span(strong("Movie analysis:")),'For live cell imaging analysis data should
                              be analyzed by Squassh as one .tiff file for all channels, slices and time points.'),
                            p(align='justify',span(strong("Channel renaming:")),'After folder selection and if Squassh data is found text 
                              fields for optional channel renaming appears.'),
                            br(),
                            br(),
                            h3('Segmentation Overview'),
                            p(align='justify',"Survey Squassh segmentation and exclude images from analysis"),
                            p(align='justify',span(strong("Squassh parameters:")),'After selecting a condition Squassh Analyst 
                              will display parameters used by Squassh for the segmentation.'),
                            p(align='justify',span(strong("Images and segmentation preview:")),'Squassh Analyst 
                              displays contact sheet previews of original microscopy images maximum intensity  z-projections
                              alongside Squassh segmentation z-projection. Cell mask if present is displayed on both images as white outline.'),
                            p(align='justify',span(strong("Image features overview:")),'Squassh Analyst 
                              displays next to contact sheets images a table containing mean number of objects in each channel and colocalization quantification (for two channel images).'),
                            p(align='justify',span(strong("Image removal:")),'Exclude from further analysis bad images by ticking  \'Exclude image box\'. '),
                            br(),
                            br(),

                            h3('Analysis'),
                            p("Compute image features and compare across experimental conditions"),
                            
                            
                            p(span(strong("Subcellular object/image attribute:")),
                            'Select wich image feature to compute and display.'),

                            
                            HTML('<ul>
                                 <li><i>Mean object volume:</i> computes the mean volume of subcellular objects.
                                  Mean is computed for each image over all its subcellular objects.
                                  For 2D images this computes the surface of subcellular objects.</li>
                                 <li><i>Mean object length:</i> computes for each image the mean length of its subcellular objects.</li>
                                 <li><i>Mean object intensity:</i> computes for each image the mean intensity of its subcellular objects.
                                    Intensities are normalized in each image between 0 and 1.</li>
                                  <li><i>Colocalization (number):</i> percentage of objects in one channel that overlap with objects from the other channel. An object is considered overlapping with the other channel if at least 50% of its volume is overlapping with objects form the other channel.</li>
                                  <li><i>Colocalization (size):</i> percentage of the total size of objects in one channel overlapping with objects from the other channel.</li>
                                  <li><i>Colocalization (signal):</i> percentage of the total signal (=volume*intensity) of objects in one channel overlapping with objects from the other channel</li>
                                  <li><i>Pearson correlation:</i> pearson correlation coefficient between two channels</li>
                                  <li><i>Pearson correlation inside cell mask:</i> pearson correlation coefficient computed inside cell masks only.</li>
                                  <li><i>Total object volume Venn diagram:</i> displays a Venn diagramm representing the overlap between the objects volumes in the different channels.
                                          This attribute can only be displayed for one experimental condition at a time. The first condition selected in \'Compare experimental conditionss\' is used.</li>
                                  <li><i>Total object signal:</i> total signal (=volume*intensity) of objects in an image. This quantitiy is extensive with the cell size : the larger a cell is, the more subcellular objects it can have.</li>
                                  <li><i>Total object signal / Cell Size:</i> same as Total object signal but normalized with the cell size. Cell size is computed from the cell mask.</li>
                                  <li><i>Total object volume:</i> total volume of objects in an image.</li>
                                  <li><i>Total object volume / Cell Size:</i> same as total object volume but normalized with cell size.</li>
                                  <li><i>Object number:</i> total number of objects in an image.</li>
                                  <li><i>Object number / Cell Size:</i> same as Object number but normalized with cell size.</li>
                                 </ul>'),
                            


                            p(align='justify',span(strong("Channel selection:")),'Select the channel in which the image attribute is computed. For three channel images, when a Pearson correlation attribute is selected the field turns into a two channel selection form. 
                              For a colocalization attribute the selection is ordered to be able to select either channnel 1 colocalizing with channel 2 or channel 2 colocalizing with channel 1.
                              In the case of three channel images and colocalization attribute it is possible to compute the colocalization of one channel with two others by selecting one channel in the first field and two in the second field.'),
                            
                            p(align='justify',span(strong("Condition selection:")),'Experimental conditions on which the image attribute will be computed. Each condition will be displayed in the chart as a separate bar or boxplot.
                              The condition selection field is non reactive, use \'Apply conditions changes\' for modifications to take effect. Use \'Select all\' or \'Deselect all\' as shortcuts to select/ deselect all conditions.'),
                            
                            p(align='justify',span(strong("Excluded images:")),'Images excluded in the \'segmentation overview\' tab are given here as a reminder.'),
                            
                            p(align='justify',span(strong("Chart type:")),'Plot image attributes with Box plot, Strip chart, or Bar chart. Bar charts are displayed with mean values and standard error of the mean. Conditions can be sorted by increasing mean value by ticking \'Sort conditions\'.'),
                            
                            
                            
                            p(align='justify',span(strong("Movie analysis:")),'In the case of movie analysis there is no chart type option. The plot displays the evolution over time of the selected image attribute.'),
                            
                            p(align='justify',span(strong("Figure export:")),'Use \'Export pdf\' button to export current chart in .pdf format. For png format export, right click the chart and save as image. 
                              Export the data corresponding to the currently displayed chart in a spreadsheet file with \'Export .csv\'.'),
                            
                            p(align='justify',span(strong("Filters:")),'Remove subcellular objects from the analysis by setting thresholds on their minimum volume, maximum volume or minimum intensities. 
                                    Remove images from the analysis by setting a threshold on the minimum number of subcellular objects an image has to contain.'),
                            
                            p(align='justify',span(strong("Most representative image:")),
                              'SuqasshAnalyst displays the image whose attribute value is closest to the median value of all images in the chosen condition. This thus provides the most representative image of the group for the calclated attribute. The file name of the image is given on top of its contact sheet preview.'),
                            
                            p(align='justify',span(strong("Statistical analysis:")),'
When at least two experimental conditions are selected statistical significance tests results are performed in this section. Two statistical analysis are possible, parametric or non parametric. The first one is one way ANOVA followed by Tukey Honest Significance difference test for multiple comparisons. The second one is the 
Kruskal-Wallis test followed by Dunn\'s test adjusted with the Bonferonni correction for multiple comparisons. ANOVA and Tukey are parametric, that is they require that samples come from Gaussian distributions. The non parametric tests Kruskal-Wallis and Dunn do not require this assumption but only take into account the rankings
of the values and have thus less statistical power. Star ratings are given next to p values with **** for p value < 0.0001, *** for 0.0001 < p value < 0.001, ** for 0.001 < p value < 0.01, * for 0.01 < p value < 0.05 and ns for p value > 0.05.
')
                            
                            
                            #sidebarLayout
                            #(
                              
                             # sidebarPanel("sidebar"),
                              
                            #  mainPanel("main plot area")
                            #)
                   )
                   
)
)

