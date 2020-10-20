select
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
    sr.ethnicity,
    2020 - sr.year_born as age,
    sr.gender,
    cn.votes_2016_clinton / cn.votes_2016_all as cn_clinton_pct,
    cn.votes_2016_trump / cn.votes_2016_all as cn_trump_pct,
    pop_2017_18plus as cn_pop
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
where sr.date_created > UNIX_TIMESTAMP(20191231)