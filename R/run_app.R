#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options

run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  utils::data("embryo_j5", package = "ISEE")
  all.data <- c("embryo_j5", "embryo_j7", "embryo_j9", "embryo_j11")
  lpfun <- iSEE::createLandingPage(
    seUI=function(id) selectInput(id, "Dataset:", choices=all.data),
    seLoad=function(x) get(x)
  )

  initial <- list()

  ################################################################################
  # Settings for Reduced dimension plot 1
  ################################################################################
  initial[["ReducedDimensionPlot"]] <- new("ReducedDimensionPlot", Type = "UMAP", XAxis = 1L, YAxis = 2L, 
    FacetRowByColData = "seurat_clusters", FacetColumnByColData = "seurat_clusters", 
    ColorByColumnData = "seurat_clusters", ColorByFeatureNameAssay = "logcounts", 
    ColorBySampleNameColor = "#FF0000", ShapeByColumnData = "seurat_clusters", 
    SizeByColumnData = "nCount_RNA", FacetRowBy = "None", FacetColumnBy = "None", 
    ColorBy = "Feature name", ColorByDefaultColor = "#000000", 
    ColorByFeatureName = "KRT18", ColorByFeatureSource = "RowDataTable1", 
    ColorByFeatureDynamicSource = FALSE, ColorBySampleName = "AAACCTGTCAATCTCT-1_1", 
    ColorBySampleSource = "---", ColorBySampleDynamicSource = FALSE, 
    ShapeBy = "None", SizeBy = "None", SelectionAlpha = 0.1, 
    ZoomData = numeric(0), BrushData = list(), VisualBoxOpen = TRUE, 
    VisualChoices = "Color", ContourAdd = FALSE, ContourColor = "#0000FF", 
    PointSize = 1, PointAlpha = 1, Downsample = FALSE, DownsampleResolution = 200, 
    CustomLabels = FALSE, CustomLabelsText = "AAACCTGTCAATCTCT-1_1", 
    FontSize = 1, LegendPointSize = 5, LegendPosition = "Bottom", 
    HoverInfo = TRUE, LabelCenters = FALSE, LabelCentersBy = "seurat_clusters", 
    LabelCentersColor = "#000000", VersionInfo = list(iSEE = structure(list(
        c(2L, 5L, 0L)), class = c("package_version", "numeric_version"
    ))), PanelId = c(ReducedDimensionPlot = 1L), PanelHeight = 500L, 
    PanelWidth = 3L, SelectionBoxOpen = FALSE, RowSelectionSource = "---", 
    ColumnSelectionSource = "---", DataBoxOpen = FALSE, RowSelectionDynamicSource = FALSE, 
    ColumnSelectionDynamicSource = FALSE, RowSelectionRestrict = FALSE, 
    ColumnSelectionRestrict = FALSE, SelectionHistory = list())
  ################################################################################
  # Settings for Row data table 1
  ################################################################################
  initial[["RowDataTable1"]] <- new("RowDataTable", Selected = "KRT18", Search = "KRT18",
    SearchColumns = c("", "", ""), HiddenColumns = character(0), VersionInfo = list(iSEE = structure(list(c(2L, 5L, 0L)),
    class = c("package_version", "numeric_version"))), PanelId = c(RowDataTable = 1L), PanelHeight = 500L, PanelWidth = 3L, 
    SelectionBoxOpen = FALSE, RowSelectionSource = "---", ColumnSelectionSource = "---", 
    DataBoxOpen = FALSE, RowSelectionDynamicSource = FALSE, ColumnSelectionDynamicSource = FALSE, 
    RowSelectionRestrict = FALSE, ColumnSelectionRestrict = FALSE, 
    SelectionHistory = list())
################################################################################
# Settings for Feature assay plot 1
################################################################################
  initial[["FeatureAssayPlot1"]] <- new("FeatureAssayPlot", Assay = "logcounts", XAxis = "Column data", 
    XAxisColumnData = "seurat_clusters", XAxisFeatureSource = "---", 
    XAxisFeatureDynamicSource = FALSE, YAxisFeatureName = "NANOG", 
    YAxisFeatureSource = "RowDataTable1", YAxisFeatureDynamicSource = FALSE, 
    FacetRowByColData = "orig.ident", FacetColumnByColData = "orig.ident", 
    ColorByColumnData = "orig.ident", ColorByFeatureNameAssay = "logcounts", 
    ColorBySampleNameColor = "#FF0000", ShapeByColumnData = "orig.ident", 
    SizeByColumnData = "nCount_RNA", FacetRowBy = "None", FacetColumnBy = "None", 
    ColorBy = "None", ColorByDefaultColor = "#000000", ColorByFeatureName = "NANOG", 
    ColorByFeatureSource = "---", ColorByFeatureDynamicSource = FALSE, 
    ColorBySampleName = "AAACCCAAGCCTATCA-1", ColorBySampleSource = "---", 
    ColorBySampleDynamicSource = FALSE, ShapeBy = "None", SizeBy = "None", 
    SelectionAlpha = 0.1, ZoomData = numeric(0), BrushData = list(), 
    VisualBoxOpen = FALSE, VisualChoices = "Color", ContourAdd = FALSE, 
    ContourColor = "#0000FF", PointSize = 1, PointAlpha = 1, 
    Downsample = FALSE, DownsampleResolution = 200, CustomLabels = FALSE, 
    CustomLabelsText = "AAACCCAAGCCTATCA-1", FontSize = 1, LegendPointSize = 1, 
    LegendPosition = "Bottom", HoverInfo = TRUE, LabelCenters = FALSE, 
    LabelCentersBy = "orig.ident", LabelCentersColor = "#000000", 
    VersionInfo = list(iSEE = structure(list(c(2L, 5L, 0L)), class = c("package_version", 
    "numeric_version"))), PanelId = c(FeatureAssayPlot = 1L), 
    PanelHeight = 500L, PanelWidth = 3L, SelectionBoxOpen = FALSE, 
    RowSelectionSource = "---", ColumnSelectionSource = "---", 
    DataBoxOpen = FALSE, RowSelectionDynamicSource = FALSE, ColumnSelectionDynamicSource = FALSE, 
    RowSelectionRestrict = FALSE, ColumnSelectionRestrict = FALSE, 
    SelectionHistory = list())
  ################################################################################
  # Settings for Complex heatmap 1
  ################################################################################
  initial[["ComplexHeatmapPlot"]] <- new("ComplexHeatmapPlot", Assay = "logcounts", CustomRows = TRUE, 
    #CustomRowsText = "Pax8\nTg\nActa2\nPou5f1\nSox2\nKrt5\nCol1a2\nPerp\nFcer1g\nFlt1\nPou3f3\n", 
    ClusterRows = FALSE, ClusterRowsDistance = "spearman", ClusterRowsMethod = "ward.D2", 
    DataBoxOpen = FALSE, VisualChoices = "Annotations", ColumnData = "seurat_clusters", 
    RowData = character(0), CustomBounds = FALSE, LowerBound = NA_real_, 
    UpperBound = NA_real_, AssayCenterRows = FALSE, AssayScaleRows = FALSE, 
    DivergentColormap = "purple < black < yellow", ShowDimNames = "Rows", 
    LegendPosition = "Bottom", LegendDirection = "Horizontal", 
    VisualBoxOpen = FALSE, NamesRowFontSize = 10, NamesColumnFontSize = 10, 
    ShowColumnSelection = TRUE, OrderColumnSelection = TRUE, 
    VersionInfo = list(iSEE = structure(list(c(2L, 5L, 0L)), class = c("package_version", 
    "numeric_version"))), PanelId = c(ComplexHeatmapPlot = 1L), 
    PanelHeight = 500L, PanelWidth = 3L, SelectionBoxOpen = FALSE, 
    RowSelectionSource = "---", ColumnSelectionSource = "---", 
    RowSelectionDynamicSource = FALSE, ColumnSelectionDynamicSource = FALSE, 
    RowSelectionRestrict = FALSE, ColumnSelectionRestrict = FALSE, 
    SelectionHistory = list())

  iSEE::ExperimentColorMap(
  assays = list(),
  colData = list(),
  rowData = list(),
  all_discrete = list(assays = NULL, colData = NULL, rowData = NULL),
  all_continuous = list(assays = NULL, colData = NULL, rowData = NULL),
  global_discrete = NULL,
  global_continuous = NULL
  )

  with_golem_options(
    app = iSEE::iSEE(landingPage=lpfun,
                    appTitle = "iSEE - Pig Embryo",
                    initial = initial,
                    bugs = FALSE),
    golem_opts = list(...)
  )
}
