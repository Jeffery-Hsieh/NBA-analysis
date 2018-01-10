# NBA_analysis

#### 變數解釋
### 數據已在GitHub上，直接將clean_data.RData Load進Rstudio
* 月份_game_analysis：各月份詳細比賽內容
    + Date：比賽日期
    + Visitor_team：客場隊伍
    + Home_team：主場隊伍
    + Winner_team：獲勝隊伍
    + 客場隊伍_box_scores：隊伍比賽分析數據
    + 主場隊伍_box_scores：同上

* game_result_table：各月份比賽隊伍與時間

#### 資料集
new_team_data：只有本隊數據

diff_data：與對手數據相減

new_diff_data：合併了隊伍名稱互換且數據加負號的資料

after_diff_svmfit：使用兩隊數據相減後的資料進行svm分類

nnetM：使用兩隊數據相減後的資料進行ann分類

從new_diff_data直接篩選主場客場隊伍的資訊，將含有usg開頭的欄位丟入進行predict，如有疑問可先參考ANN.R的做法
