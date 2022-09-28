# Tests based on Erik Well's check

if (requireNamespace("LCVP", quietly = TRUE)) {

    
  # Somewhat puzzling is the often low matching rate for completely simple taxa
  # (from a European perspective). This concerns pure "genus+species" input
  # cases without authors. E.g. "Artemisia vulgaris", "Artemisia biennis", ... .
  # As far as I understood this comes into play when there is at least any one
  # unresolved homonym out there. Are these homonymes checked alphabetically?
  # And simply the first match is taken? Like somehow ... by chance?
  test_that("Artemisia vulgaris, Artemisia biennis", {
    #sp1
    sp <- "Artemisia vulgaris"
    expect_warning(res_ex <- lcvp_search(sp))
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 1)
    res_ex <- lcvp_fuzzy_search(sp)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 4)
    # sp2
    sp <- "Artemisia biennis"
    expect_warning(res_ex <- lcvp_search(sp))
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 1)
    res_ex <- lcvp_fuzzy_search(sp)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 2)
  })
  
  
  # Similarly, when querying “Myosotis micrantha” a very common, well-known
  # synonym for “M. stricta”, lcvp_search returns “Myosotis sicula” just because
  # “M. micrantha Guss.” comes before “M. micrantha Pall. ex Lehm.” ?? It seems
  # this problem - while easy for botanists - is hard for machines and
  # ecological number crunchers alike. Would be a weighting scheme a solution?
  # Number of sources listing a certain name? Then the “Europe bias” is
  # introduced of course...
  test_that("Myosotis micrantha", {
    #sp1
    sp <- "Myosotis micrantha"
    expect_warning(res_ex <- lcvp_search(sp))
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 1)
    res_ex <- lcvp_fuzzy_search(sp)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 2)
    
  })


# More complicated: in the case of "Artemisia campestris" lcvp_search leads to
# an unresolved due to "A. campestris KITAM." Why not to A. campestris LEDEB.
# (A. pubescens LEDEB.) or A. campestris SCOP. EX STEUD. (A. gallica SCHUR) or
# A. campestris TURCZ. EX DC. (A. ledebouriana BESSER)? All these would then
# lead to certain other output suggestions? Of course, homonyms without Authors
# can be very ambiguous. But to obtain an "unresolved" for simple species like
# those might leave many potential users unsatisfied. TNRS had this problem too,
# but nowadays the most probable accepted output taxon is returned and the
# remaining n matches can be unfolded.

  test_that("Artemisia campestris", {
    #sp1
    sp <- "Artemisia campestris"
    expect_warning(res_ex <- lcvp_search(sp))
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 1)
    res_ex <- lcvp_fuzzy_search(sp)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 7)
    
  })
  
  # Furthermore, with lcvp_fuzzy_search if I just select a column of a dataframe
  # for a character vector=species list, this results in an error message:
  # Fehler in names(gen_pos_mult) <- LCVP::tab_position$Genus[gen_pos] :
  # Attribut 'names' [33116] muss dieselbe Länge haben wie der Vektor [1] Even
  # if I load a single plain text column and select it as (splist=TABLE$X1) I
  # get the very same error message. From the documentation I know the
  # requirement to have each element including genus
  # test_that("lcvp_fuzzy_search works for tab names", {
  # sp <- read.csv("inst/extdata/test.csv")
  # res_ex <- lcvp_fuzzy_search(sp[, "Scientific.Name"])
  # })
    
  # In general, it would be better if "spec." and "agg." would be recognized as
  # such, especially when otherwise author names are present. The fuzzy match is
  # dangerous in such cases. For example, a "Malva spec." in the Flora list of
  # Halle (Stefan Klotz) fuzzily becomes "Malva spicata L." and via the synonym
  # the Halle flora gets the neotropical "Melochia spicata (L.) FRYXELL" that
  # might go straight into GloNaf as a new alien invader of Central Europe
  test_that("spec. and agg.", {
    sp <- "Malva spec." 
    expect_warning(res_ex <- lcvp_search(sp))
    expect_true(is.null(res_ex))
  })
  
  
  # Also, in many checklists there is still a space after the hybrid "x". LCVP
  # only recognizes "Platanus xhispanica Miller ex Münchh." but not the very
  # common "Platanus x hispanica Miller ex Münchh.". It would be advisable to
  # recognize "space-x-space".
  test_that("x hybrid", {
    sp <- "Platanus xhispanica Miller ex Münchh."
    res_ex <- lcvp_search(sp)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 1)
    sp <- "Platanus x hispanica Miller ex Münchh."
    expect_warning(res_ex <- lcvp_search(sp))
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 1)
  })
  
  # A serious problem seems to be that clear names get misinterpreted and
  # thereby wrong taxa are introduced: Achillea macrophylla L. becomes Aspilia
  # platyphylla (BAKER) S.F.BLAKE Achillea roseo-alba EHREND. becomes Aspilia
  # subscandens J.U.SANTOS
  test_that("misinterpreted ", {
    sp <- "Achillea macrophylla L."
    res_ex <- lcvp_search(sp)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 1)
    sp <- "Achillea roseo-alba EHREND"
    res_ex <- lcvp_search(sp)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 1)
  })
  
  # It’s not clear to me if the join algorithm works fuzzy or not. E.g., when I
  # check for the slightly misspelled Carex riparia CURT. (instead of CURTIS)
  # lcvp_join translates to Uncinia riparia R.BR. (Carex sclerophylla (NELMES)
  # K.L.WILSON). Here, again, it seems like just the first match is taken as
  # granted (Carex riparia (R.BR.) POIR.).
  test_that("join ", {
    sp <- "Carex riparia CURT."
    expect_warning(res_ex <- lcvp_search(sp))
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 1)
    
    res_ex <- lcvp_fuzzy_search(sp)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 5)
  })
  
  # Different case: Alchemilla hoppeana (RCHB.) DALLA TORRE, with the correct,
  # earlier combination by DALLA TORRE in a Bavarian test list gets
  # (incorrectly) recognized as the later Alchemilla hoppeana (RCHB.) BUSER -
  # but then translated to A. alpigena HEGI which is clearly a different taxon
  # of the plicatula aggregate. Moreover, according to the fuzzy search,
  # Alchemilla hoppeana (RCHB.) DALLA TORRE is the accepted output taxon.
  test_that("Alchemilla ", {
    sp <- "Alchemilla hoppeana (RCHB.) DALLA TORRE"
    expect_warning(res_ex <- lcvp_search(sp))
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 1)
    
    res_ex <- lcvp_fuzzy_search(sp)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 2)
  })
  
  
  # Even if author names are provided, they seem to be ignored at least partly
  # (with subspecies?). Looking up Cerastium arvense L. subsp. arvense with
  # lcvp_search reveals Cerastium orithales SCHLTDL. which is a synonym of the
  # illegitim C. arvensis CHAM & SCHLTDL. Here the “L.” in the input seems to be
  # ignored. Similar with the input “Festuca arundinacea SCHREB. subsp.
  # arundinacea” which is mistaken as Festuca arundinacea LILJ. == Scolochloa
  # festucacea (WILLD.) LINK even if “Festuca arundinacea SCHREB.” as such is
  # correctly recognized (Lolium arundinaceum (SCHREB.) DARBYSH.).
  test_that("ignored authors ", {
    sp <- "Cerastium arvense L. subsp. arvense"
    res_ex <- lcvp_search(sp)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 1)
    
    sp <- "Festuca arundinacea SCHREB. subsp. arundinacea"
    res_ex <- lcvp_search(sp)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 1)
  })
  
  # Maybe the category homonym is lacking completely? Homonyms are rarely
  # synonyms. E.g. “Prunus padus L. subsp. padus” has a lot of “accepted” names
  # in the lcvp_fuzzy_search output. These are taxa obviously different from
  # each other and Prunus padus L. One of the synonyms, Prunus padus BRANDIS is
  # rather a homonym, however then gets translated into the lcvp_search output
  # Prunus cornuta (WALL. ex ROYLE) STEUD. which is a species of India.
  test_that("homonyms again ", {
    sp <- "Prunus padus L. subsp. padus"
    res_ex <- lcvp_search(sp)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 1)
    
    res_ex <- lcvp_fuzzy_search("Prunus padus", 
                         status = "accepted") 
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 1)
  })
  
  
  # Other (casually found) examples include Marsdenia chirindensis which was
  # newly named in 2020 by David Goyder (Kew – so a quite visible and reliable
  # description). By matching it with Marsdenia chinensis, lcvp_search wrongly
  # translates it to Jasminanthes mucronates. Even the famous new description of
  # Tiganophyton karasense which even constitutes a new Brassicaceae family
  # (Tiganophytaceae) is not contained in LCVP 2.0. More inconspicuously
  # described new species like Polygala mazandaranica (Iran) or Allium
  # arampatzisii (N-Greece) are also missing, so it might be a good idea to
  # provide a date until which new described species are included, especially
  # regarding the showy and mouthwatering press releases.
  test_that("Marsdenia chirindensis  ", {
    sp <- "Marsdenia chirindensis"
    res_ex <- lcvp_search(sp)
    expect_equal(class(res_ex), "data.frame")
    expect_equal(ncol(res_ex),14)
    expect_equal(nrow(res_ex), 1)
  })
  
  
}
