# Georgian morphology

Common Lisp and Xerox FST code to generate (Old, Middle, Modern) Georgian finite state transducers for morphosyntax.


How to build the fst morphology:

Load the system kartuli-paradigm.asd

(Reload paradigm-www.lisp to get web page encoding right.)

eval the file utp-to-fst.lisp;
run #+main-ccl in that file;

write-fst-noun-stems-sql()
write-fst-participle-stems-sql() ; contains VN
write-verb-lexicon-regexp()
  verb-lexicon-regexp()
    get-root-override-features()
see verb-feature-table-sql.lisp: get-root-override-features() for vn etc.

write-verb-regex-file()
write-noun-regex-file()

fst to foma: see regex/syntax.regex

URLs:

http://gekko.local/kartuli/roots
http://gekko.local/kartuli/masdars
http://gekko.local/kartuli/nouns

Database: localhost/gnc/gnc/kartuli

The basic layout is like this:

Every row in verb_paradigm describes part of a paradigm, where a paradigm as a whole is identified by id and sub_id.

This table is n:1 linked to the table verb_features, which contains information related to more than one paradigm. The linking structure is explained in more detail below.

verb_paradigm:

id: root id
sub_id: paradigm sub_id, not the same as sub_id in verb_features!
c_root: common root
vn: verbal nouun
impf_vn: verbal noun for the imperfective form
pf_vn: verbal noun for the perfective form
tsch_class: Tschenkéli verb class
features_sub_id: links the row to a row (identified by id, sub_id) in verb_features
link_sub_id: (obsolete)
base_sub_id: links to the base paradigm (that is, e.g. active paradigm for passive and causative forms, etc.)
participle_sub_id: links to (id, sub_id) in table participle 
pv: preverb
pf_pv: perfective preverb
impf_pv: imperfective preverb
dir_pv_p: directional preverb
red_dir_pv: reduplicated directional preverb
comment 
author 
source 
derived_type 
date 
accepted 
pf_12_pv: perfective preverb for 1. and 2. person
no_preverbless_aor 
class 
disabled

verb_features:

unique_id: a unique identifier for the row
id: the root id (the same that you find on the page https://clarino.uib.no/kartuli/roots)
sub_id: the id of the paradigm
root: the root of the forms defined by the row
c_root: the common root (as on https://clarino.uib.no/kartuli/roots)
tense: the tenses for this row
pv: the preverb (obsolete; see verb_paradigm)
vn: the associated verbal noun
gv: genus verbi
sf: stem formant
caus_sf: causative suffix
vv: version vowel (ქცევა)
tsch_class: verb class according to Tschenkéli’s dictionary
morph_type: morphology type
relation: absolute or relative
reduplication: (obsolete; see verb_paradigm))
red_dir_pv: (obsolete, see; see verb_paradigm) 
stem_type: (obsolete)
pr_st_ext: present stem extension
part_pfx: participle prefix
part_sfx: participle suffix
passive_sfx
nasal_infix
type_aorist: aorist type
type_obj_3_pfx: prefix type of 3. obj person
type_aorist_3sg
type_optative
subj_pers: subject persons for this row, if not all
subj_num
obj_pers
type_subj12_sfx
type_subj3_sfx
type_subj2_pfx
type_ev_sfx
style
type_pr_st_ext
paradigm_replacement: pointer to a suppletive form, by unique_id
deleted
lang
