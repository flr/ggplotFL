# test-plot.R - DESC
# /test-plot.R

# Copyright European Union, 2019
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# -- FLQuant {{{
test_that("plot(FLQuant) works as expected", {

  # dim c(1, 61, 1, 1, 1, 1)
  expect_is(plot(catch(ple4)), "ggplot")
  
  # dim c(1, 61, 1, 1, 1, 300)
  expect_is(plot(rnorm(300, catch(ple4), catch(ple4)/2)), "ggplot")
  
  # dim c(10, 61, 1, 1, 1, 1)
  expect_is(plot(catch.n(ple4)), "ggplot")
  
  # dim c(1, 61, 1, 1, 1, 300)
  expect_is(plot(rnorm(300, catch.n(ple4), catch.n(ple4)/2)), "ggplot")
  
  # dim c(1, 61, 2, 1, 1, 1)
  expect_is(plot(catch(ple4sex)), "ggplot")
  
  # dim c(1, 61, 2, 1, 1, 300)
  expect_is(plot(rnorm(300, catch(ple4sex), catch(ple4sex) / 2)), "ggplot")
  
  # dim c(1, 61, 2, 1, 2, 1)
  expect_is(plot(expand(catch(ple4sex), area=1:3)), "ggplot")
  
  # dim c(1, 61, 2, 1, 2, 300)
  expect_is(plot(rnorm(300, expand(catch(ple4sex), area=1:3), 1e5)), "ggplot")
  
  # dim c(3, 61, 2, 1, 2, 1)
  expect_is(plot(expand(catch.n(ple4sex)[1:3,], area=1:3)), "ggplot")
  
  # dim c(3, 61, 2, 1, 2, 300)
  expect_is(plot(rnorm(300, expand(catch.n(ple4sex)[1:3,], area=1:3),
    1e5)), "ggplot")

}) # }}}

# -- FLQuants

# -- FLStock {{{
test_that("plot(FLStock) works as expected", {

  expect_is(plot(ple4), "ggplot")
  
  expect_is(plot(ple4sex), "ggplot")

}) # }}}

# -- FLStocks

# --- probs


# --- geom_hash

# ADD the md5 hash of the input (FLR) object to a plot
#  - As small light text
#  - As attribute to the ggplot object

library(digest)
digest(ple4)

plot(ple4) +
  geom_text(data=data.frame(year=1958,data=0,label=digest(ple4), qname="F"),
    aes(label=label), colour="#BBBBBB", size=3)

plot(ple4) +
  theme(plot.margin=margin(t = 8, r = 0, b = 0, l = 0, unit = "pt")) +
  annotation_custom(grid::textGrob(digest(ple4), x=unit(0.85, "npc"),
    y = unit(0.99, "npc"), just = "right"))
   
plot(ple4) +
  labs(title=digest(ple4))
