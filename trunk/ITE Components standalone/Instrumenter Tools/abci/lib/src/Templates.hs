{-# LANGUAGE TemplateHaskell #-}
module Templates where

--
-- NOTE
--
-- Touch this file to force recompilation,
-- if any of the templates change
--

import Text.HTML.Chunks


-- Code generation templates
$( chunksFromFile "src/templates/InhInfo.tas" )

-- Catalog.xml templates
$( chunksFromFile "src/templates/catalog.txml" )
