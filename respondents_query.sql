select
    pd.week,
    pd.state_abbrev,
    pd.county_descr,
    pd.fips_code,
    pd.biden,
    pd.trump,
    pd.clinton_biden,
    pd.trump_trump,
    pd.clinton_trump,
    pd.trump_biden,
    ftb.biden_ft,
    ftt.trump_ft,
    pd.prevtrump,
    pd.prevclinton,
    pd.ethnicity,
    pd.four_yr_degree,
    pd.age,
    pd.male,
    pd.county_clinton,
    pd.county_trump,
    pd.county_trump_margin,
    pd.county_pop
from
(select
    sr.id,
    FROM_UNIXTIME(sr.date_created,'%Y-%U') as week,
    z.state_abbrev,
    cn.county_descr,
    cn.fips_code,
    if(c.text like '%Biden%', 1, 0) as biden,
    if(c.text like '%Trump%', 1, 0) as trump,
    if(sr.president_vote_2016 like '%Clinton%' and c.text like '%Biden%', 1, 0) as clinton_biden,
    if(sr.president_vote_2016 like '%Trump%' and c.text like '%Trump%', 1, 0) as trump_trump,
    if(sr.president_vote_2016 like '%Clinton%' and c.text like '%Trump%', 1, 0) as clinton_trump,
    if(sr.president_vote_2016 like '%Trump%' and c.text like '%Biden%', 1, 0) as trump_biden,
    if(sr.president_vote_2016 like '%Clinton%', 1, 0) as prevclinton,
    if(sr.president_vote_2016 like '%Trump%' , 1, 0) as prevtrump,
    sr.ethnicity,
    2020 - sr.year_born as age,
    sr.education in ('Bachelor\'s degree, or four-year college degree', 'Graduate degree') as four_yr_degree,
    if(sr.gender like 'Male', 1, 0) as male,
    cn.votes_2016_clinton / cn.votes_2016_all as county_clinton,
    cn.votes_2016_trump / cn.votes_2016_all as county_trump,
    (cn.votes_2016_trump - cn.votes_2016_clinton) / cn.votes_2016_all as county_trump_margin,
    pop_2017_18plus as county_pop
from survey_data.survey_responders sr
    join survey_data.surveys s
        on sr.survey_id = s.id
    join survey_data.survey_responses r
        on sr.id = r.survey_responder_id
    join survey_data.survey_questions q
        on q.id = r.survey_question_id
        and q.heading in (
        'If the election for President were held today, who would you vote for?',
        'If the 2020 general election for President were held today and the candidates were the following, who would you vote for?',
        'If the election for President were held today and the candidates were the following, who would you vote for?',
        'If the November election for President were held today, who would you vote for?'
        )
    join survey_data.survey_choices c
        on c.id = r.survey_choice_id
        and c.survey_question_id = q.id
    join (select ZCTA, state_abbrev, association_id from zip_map where association_type = 'county' group by ZCTA) z
        on z.ZCTA = sr.zip_code
    join counties cn
        on z.state_abbrev = cn.state_abbrev
        and z.association_id = cn.county_code
where sr.date_created > UNIX_TIMESTAMP(20191231)) as pd
join (select
                  sr.id,
                  c.text as biden_ft
from survey_data.survey_responders sr
    join survey_data.surveys s
        on sr.survey_id = s.id
    join survey_data.survey_responses r
        on sr.id = r.survey_responder_id
    join survey_data.survey_questions q
        on q.id = r.survey_question_id
        and q.heading in (
        'On a scale of 1-10, how do you feel about Joe Biden? 1 means you strongly oppose him and 10 means you strongly support him.'
        )
    join survey_data.survey_choices c
        on c.id = r.survey_choice_id
        and c.survey_question_id = q.id
where sr.date_created > UNIX_TIMESTAMP(20191231)
) ftb on pd.id = ftb.id
join (select
             sr.id,
             c.text as trump_ft
from survey_data.survey_responders sr
    join survey_data.surveys s
        on sr.survey_id = s.id
    join survey_data.survey_responses r
        on sr.id = r.survey_responder_id
    join survey_data.survey_questions q
        on q.id = r.survey_question_id
        and q.heading in (
        'On a scale of 1-10, how do you feel about President Donald Trump? 1 means you strongly oppose him and 10 means you strongly support him.'
        )
    join survey_data.survey_choices c
        on c.id = r.survey_choice_id
        and c.survey_question_id = q.id
where sr.date_created > UNIX_TIMESTAMP(20191231)
) ftt on pd.id = ftt.id