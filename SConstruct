import os
import sys
from os.path import join,split,splitext
import shutil
import re
import itertools
import subprocess
import pprint
import glob
import collections
import ConfigParser

# check timestamps before calculating md5 checksums
# see http://www.scons.org/doc/production/HTML/scons-user.html#AEN929
Decider('MD5-timestamp')

#### define variables and environment
vars = Variables(None, ARGUMENTS)
vars.Add('constants', 'globally used variables', 'data/constants.R')
vars.Add('functions', 'shared functions', 'scripts/functions.R')
vars.Add('nproc', 'Number of concurrent processes for resampling experiment', default=6)

varargs = dict({opt.key: opt.default for opt in vars.options}, **vars.args)
nproc = int(varargs['nproc'])

# NPROC provides number of processes for auc_distribution.R
env = Environment(ENV=dict(os.environ, NPROC=nproc), variables=vars)
Help(vars.GenerateHelpText(env))

### Builders

# builder for R scripts specifying both sources and targets
# first source should be the name of the script
env['BUILDERS']['RScript'] = Builder(
    action='R -q --slave -f ${SOURCES[0]} --args $TARGETS ${SOURCES[1:]}'
    )

# sweave
def sweave_generator(source, target, env, for_signature):
    dirname, fname = split(str(source[0]))

    action = ''
    if dirname:
        action += 'cd "%s" && ' % dirname

    action += r'echo Sweave\(\"%s\"\) | R --slave' % fname

    # action += 'R CMD Sweave "%s"' % fname

    if len(source) > 1:
        action += ' --args ${SOURCES[1:]}'

    return action

env['BUILDERS']['sweave'] = Builder(
    generator=sweave_generator,
    emitter=lambda target, source, env: (splitext(str(source[0]))[0]+'.tex', source)
)


# stangle
def stangle_generator(source, target, env, for_signature):
    dirname, fname = split(str(source[0]))

    action = ''
    if dirname:
        action += 'cd "%s" && ' % dirname

    action += 'R CMD Stangle "%s"' % fname

    return action

env['BUILDERS']['stangle'] = Builder(
    generator=stangle_generator,
    emitter=lambda target, source, env: (splitext(str(source[0]))[0]+'.R', source)
)

##### perform the analysis
data, locations = env.RScript(
    target = ['output/data.rda','output/locations.rda'],
    source = Flatten(['scripts/data.R',
                      'data/delta_data.txt.bz2',
                      'data/Locations.rda'
                      ])
    )

desc = env.RScript(
    target = 'output/description.rda',
    source = Flatten(['scripts/describe_data.R',
                      data,
                      '$constants'])
    )


samples1 = env.RScript(
    target = 'output/samples1.rda',
    source = Flatten(['scripts/expt1_samples.R',
                      '$functions',
                      data
                      ])
    )

expt1 = env.RScript(
    target = 'output/expt1.rda',
    source = Flatten(['scripts/expt1_assembly.R',
                      '$constants',
                      '$functions',
                      data,
                      samples1
                      ])
    )


cutoffs1 = env.RScript(
    target = 'output/cutoffs1.rda',
    source = Flatten(['scripts/expt1_cutoffs.R',
                      '$constants',
                      '$functions',
                      expt1
                      ])
    )

roc = env.RScript(
    target = 'output/roc.rda',
    source = Flatten(['scripts/expt1_roc.R',
                      '$constants',
                      '$functions',
                      expt1
                      ])
    )

performance = env.RScript(
    target = 'output/performance.rda',
    source = Flatten(['scripts/expt1_performance.R',
                      '$functions',
                      cutoffs1, ## 1
                      roc, ## 2
                      samples1, ## 3
                      data ## 4
                      ])
    )

## calculate distribtions of AUCs
auc_distribution = env.RScript(
    target = 'output/auc_distribution.rda',
    source = Flatten(['scripts/auc_distribution.R',
                      '$constants',
                      '$functions',
                      data
                      ])
    )
Alias('aucdist', auc_distribution)

### figures
Default('figs')
oversampling_data, fig_oversampling = env.RScript(
    target = Flatten(['output/oversampling.rda','figs/oversampling.jpg']),
    source = Flatten(['scripts/checkOversampling.R', data, samples1])
    )

fig_auc = env.RScript(
    target = 'figs/roc.pdf',
    source = Flatten([
            'scripts/figure_roc.R',
            roc,
	    performance])
    )

fig_bwplots = env.RScript(
    target = 'figs/bwplots.pdf',
    source = Flatten([
            'scripts/figure_bwplots.R',
            roc,
            expt1,
            cutoffs1])
    )

fig_boxplot = env.RScript(
    target = 'figs/auc_boxplot.pdf',
    source = Flatten([
    	     'scripts/figure_auc_boxplot.R',
             auc_distribution,
	     roc
	     ])
    )

## tab_auc = env.RScript(
##     target = 'figs/auc_tab.tex',
##     source = Flatten([
##             'scripts/table_auc.R',
##             roc,
##             cutoffs1,
##             auc_distribution
##             ])
##     )

tab_big = env.RScript(
    target = 'figs/bigtab.tex',
    source = Flatten([
            'scripts/table_bigtab.R',
            performance
            ])
    )

# assemble the report
intro = env.sweave('sec1_intro.Rnw')
methods = env.sweave(Flatten(['sec2_methods.Rnw', desc, samples1]))
results = env.sweave('sec3_results.Rnw')
discussion = env.sweave('sec4_discussion.Rnw')
legends = env.sweave('sec45_legends.Rnw')
supplement = env.sweave('sec5_supplement.Rnw')
figures_and_tables = env.sweave(
    Flatten(['figures_and_tables.Rnw',
             fig_auc,
             fig_bwplots,
             tab_big,
             fig_oversampling,
             fig_boxplot
             ]))

tex = env.sweave(
    Flatten([
            'paper.Rnw',
            intro, methods, results, discussion, legends, supplement,
            figures_and_tables
            ]))

pdf, cruft = env.Command(
    target = Flatten(['paper.pdf', Dir('paper.t2d')]),
    source = tex,
    action = 'rm -f $TARGET && scripts/texi2dvi --pdf --tidy $SOURCE'
    )
Default(pdf)
