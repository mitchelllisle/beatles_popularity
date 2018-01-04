CREATE TABLE performers (
  id varchar,
  performer varchar);

CREATE TABLE mainCategories (
  id varchar,
  mainCategory varchar);

CREATE TABLE secondaryCategories (
  id varchar,
  secondaryCategory varchar);

CREATE TABLE articles (
  id varchar,
  articleName varchar,
  mainCategoryId varchar,
  secondaryCategoryId varchar,
  dateOfInception varchar,
  dateOfBirth varchar,
  performerId varchar);
