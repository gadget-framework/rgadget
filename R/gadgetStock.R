# gadget-stockfile and related classes
# 
# Break up each component of the stockfile into it's own S4 class, use
# gadget-stockfile to contain them all.
#
# Example: Create a stock from scratch:- 
# gadget_stock_skeleton('stock_name', age = c(10, 50)) %>%
#     component('spawning', spawnsteps = 1) %>%
#     component('renewal', NULL) %>%
#     gadget_dir_write(gd)
#
# Example: Read in a stock file, modify and write back
# gadget_read_stock('stock_name', gd) %>%
#     component('renewal', NULL) %>%
#     gadget_dir_write(gd)
#
# Where fhe following functions are defined thus:-
# * component(stock, name, ...) at simplest is:-
#     slots(stock, name) <- new(paste0('gadget-stockfile-', name), ...)
# * component(stock, NULL) is approximately:-
#     slots(stock, name) <- new(paste0('gadget-stockfile-', name), does(name) = 0)
# * gadget_stock_skeleton(name, ...) is extracted from gadget.skeleton,
#   generating S4 class.
# * gadget_read_stock(name, gd) reads mainfile to work out which
#   stockfile to read, then loads the stockfile into S4 class.
# * gadget_dir_write updates stockfile, mainfile and any other dependencies.
#
# The current MFDB functions:-
# * gadget_stockfile_refweight(stock_name, data)
# * gadget_stockfile_initialconditions(stock_name, data)
# * gadget_stockfile_recruitment(stock_name, data)
# ...could have invoked via:-
# * component('refweight', data = data)
# * component('spawning', data = data)
# * component('recruitment', data = data)
# In this case, having a "data" parameter invokes gadget_stockfile_refweight() or
# equivalent to build the S4 class.

setClass('gadget-stockfile',
    representation(
        stockname = 'character',
        livesonareas = 'numeric',
        minage = 'numeric',
        maxage = 'numeric',
        minlength = 'numeric',
        maxlength = 'numeric',
        dl = 'numeric',
        refweight  = 'data.frame',
        growthandeatlengths = 'data.frame',

        growth = 'gadget-stockfile-growth',
        consumption = 'gadget-stockfile-consumption',
        initialconditions = 'gadget-stockfile-initialconditions',
        migration = 'gadget-stockfile-migration',
        maturation = 'gadget-stockfile-maturation',
        movement = 'gadget-stockfile-movement',
        renewal = 'gadget-stockfile-renewal',
        spawning = 'gadget-stockfile-spawning',
        straying = 'gadget-stockfile-straying'),
    prototype(
        stockname = '',
        livesonareas = NULL,
        minage = NULL,
        maxage = NULL,
        minlength = NULL,
        maxlength = NULL,
        dl = NULL,
        refweight  = data.frame(),
        growthandeatlengths = data.frame(),

        growth = new('gadget-stockfile-growth'),
        consumption = new('gadget-stockfile-consumption'),
        initialconditions = new('gadget-stockfile-initialconditions'),
        migration = new('gadget-stockfile-migration'),
        maturation = new('gadget-stockfile-maturation'),
        movement = new('gadget-stockfile-movement'),
        renewal = new('gadget-stockfile-renewal'),
        spawning = new('gadget-stockfile-spawning'),
        straying = new('gadget-stockfile-straying')))

setClass('gadget-stockfile-growth',
    representation(
        doesgrow = 'numeric',
        growth = 'gadget-growth',
        naturalmortality = 'vector'),
    prototype(
        doesgrow = 1,
        growth = new('gadget-growth'),
        naturalmortality = NULL))

setClass('gadget-stockfile-consumption',
    representation(
        iseaten = 'numeric',
        preyinfo = 'gadget-prey',
        doeseat = 'numeric',
        predator = 'gadget-predator'),
    prototype(
        iseaten = 1,
        preyinfo = new('gadget-prey'),
        doeseat = 0,
        predator = new('gadget-predator'))

setClass('gadget-stockfile-initialconditions',
    representation(
        initialconditions = 'list',
        initialdata = 'data.frame'),
    prototype(
        initialconditions = list(),
        initialdata = data.frame()))

setClass('gadget-stockfile-migration',
    representation(
        doesmigrate = 'numeric',
        yearstep = 'data.frame',
        migrationmatrix = 'array',
        migrationratio = 'list'),
    prototype(
        doesmigrate = 1,
        yearstep = data.frame(),
        migrationmatrix = array(),
        migrationratio = data.frame()))

setClass('gadget-stockfile-maturation',
    representation(
        doesmature = 'numeric',
        maturityfunction = 'character',
        maturestocksandratios = 'character',
        coefficients = 'character',
        maturitysteps = 'character',
        maturitylengths = 'character'),
    prototype(
        doesmature = 1,
        maturityfunction = '',
        maturestocksandratios = '',
        coefficients = '',
        maturitysteps = '',
        maturitylengths = ''))

setClass('gadget-stockfile-movement',
    representation(
        doesmove = 'numeric',
        transitionstocksandratios = 'character',#'data.frame',
        transitionstep = 'numeric'),
    prototype(
        doesmove = 1,
        transitionstocksandratios = '',#data.frame(),
        transitionstep = 0))

setClass('gadget-stockfile-renewal',
    representation(
        doesrenew = 'numeric',
        renewal = 'list',
        renewal.data = 'data.frame',
    prototype(
        doesrenew = 1,
        renewal = list(),
        renewal.data = data.frame(),

setClass('gadget-stockfile-spawning',
    representation(
        spawning = 'gadget-spawning',
        spawnsteps = 'numeric',
        spawnareas = 'numeric',
        firstspawnyear = 'numeric',
        lastspawnyear = 'numeric',
        spawnstocksandratio = 'list',
        proportionfunction = 'vector',
        mortalityfunction = 'vector',
        weightlossfunction = 'vector',
        recruitment = 'vector',
        stockparameters = 'data.frame'),
    prototype(
        doesspawn = 0,
        spawnsteps = 0,
        spawnareas = 0,
        firstspawnyear = 0,
        lastspawnyear = 0,
        spawnstocksandratio = list(),  # stock = ratio
        proportionfunction = c(func = 'constant', alpha = 1),
        mortalityfunction = c(func = 'constant', alpha = 0),
        weightlossfunction = c(func = 'constant', alpha = 0),
        recruitment = c(func = 'simplessb', mu = 1),
        stockparameters = data.frame(mean = NULL, sttdev = NULL,
                                     alpha = NULL, beta = NULL)))

setClass('gadget-stockfile-straying',
    representation(
        doesstray = 'numeric',
        straysteps = 'numeric',
        strayareas = 'numeric',
        straystocksandratios = 'vector',
        proportionfunction = 'vector'),
    prototype(
        doesstray = 0,
        straysteps = 'numeric',
        strayareas = 'numeric',
        straystocksandratios = 'vector',
        proportionfunction = 'vector'))
