实验1A数据及变量说明
#Exp1A_ImageFeatures_148x19.csv: 包含了实验 1A 中所有 148 幅艺术品的 19 个计算图像特征的数据文件
#Exp1A_NatDis_testSorted_27ss.csv: 自然度和美学评分的“测试”数据
#Exp1A_NatDis_retestSorted_27ss.csv: 自然度和美学评分的“测试”和“重测”数据，用于重复的刺激
Subj，jatosResID: 被试编号
image: 图像编号
Block:
  1 (t1_x1): 对审美吸引力的评分（148张图像）
  2 (t1_x2): 对自我相关性的评分（148张图像）
  3 (t2_x1): 对审美吸引力的再评分（20张具有代表性的图像，用于重测）
  4 (t2_x2): 对自我相关性的再评分（20张具有代表性的图像，用于重测）
Question：评分的两个问题, 1表示审美吸引力，2表示自我相关性（self-relevance）
TestRe: 是否重测，1表示首次测试，2表示重测
Trial: 试次
Image：图像编号
Rating：评分
Time：反应时


实验1B数据及变量说明
# Exp1B_ImgData_testSorted_243ss.csv：包含了 243 名参与者对 42 张图像的评分数据
jatosResID：受试者 ID（参与者编号）
image：图像编号
trial_main：主要区块中的试次编号。主要区块包括观看图像，然后对每个图像进行多次评分
mainRT：回答所有评分并点击“下一个”所用的总时间
Moved：“被感动”的评分
Beauty：“美感”的评分
trial_sr：区块二中的试次编号。区块二包括观看图像，然后对自我相关性进行评分
srRT：进行评分并点击“下一个”所用的总时间
SelfRelev：自我相关性评分
（主要模块：被试观看每张图片，时间为5s，然后要求被试在回答图片在多大程度上感受到了感动和美感
模块二：被试以随机的顺序再次看到每幅作品，并回答图像的自我相关性如何）

# Exp1B_QData_243ss.csv：实验1B的人口统计、情绪和参与者的其他数据 
jatosResID: 被试编号
Consent: 被试知情同意情况
Age: 年龄
Gender: 性别，0表示未说明性别，1表示男性，2表示女性，3表示其他性别
Hand: 利手，1表示右利手，2表示左利手
Vis: 视力，1表示正常视力， 1表示矫正至正常
Neuro: 神经系统疾病，1表示被诊断患有神经系统疾病，2表示没有已知的神经系统疾病
Neuro_comm: 神经系统疾病具体解释
Med: 用药，1表示因神经系统疾病在用药，2表示目前未因神经系统疾病在用药
Educ: 受教育水平
Training: 艺术、音乐、设计、舞蹈、表演、创意写作的培训
catchTrial: 注意检查，0表示没有正确回答注意检查试验，1表示正确回答注意检查试验

Aesthetic Responsiveness Assessment: 审美反应评估
area_AA: AReA 美感吸引力子得分
area_IAE: AReA 强烈美感体验子得分
area_CB: AReA 创造性行为子得分
area_ARTot: AReA 总分

Positive and Negative Affect Schedule (PANAS): 正性负性情绪量表
panas_PA: PANAS: 正性情绪得分
panas_NA: PANAS: 负性情绪得分

Big Five Inventory: 大五人格问卷
bfi_soc: 社交性
bfi_ass: 自信
bfi_enl: 精力充沛
bfi_com: 同情心
bfi_rse: 尊重他人
bfi_tru: 信任
bfi_org: 组织性
bfi_pro: 生产力
bfi_rsp: 责任心
bfi_anx: 焦虑
bfi_dep: 抑郁
bfi_emv: 情绪波动
bfi_int: 智慧好奇心
bfi_aes: 美感敏感度
bfi_cre: 创造力想象力
bfi_Ext: 外向性
bfi_Agr: 宜人性
bfi_Con: 尽责性
bfi_Neg: 消极情绪性
bfi_Opn: 开放性


实验2数据及变量说明
#Dataratings_Final.csv: 包含了 N=40 名通过测试-重测阈值的参与者的数据，按区块组织，并添加了条件类别和图像名称
Subj: 受试者编号
Trial: 试次编号
b1: 审美吸引力评分（80 张图像）
b2: 审美吸引力评分（80 张图像，重测）
b3: 自我相关性评分（80 张图像）
b4: 自我相关性评分（80 张图像，重测）
Cat: 图像条件（类别）
SR: 自我相关
NR: 非自我相关， 在文章中重命名为Other Relevant（OR）
RA: 真实（人工制作）艺术品
GA: 生成的控制艺术品，在文章中重命名为Generated Control (GC)
Imgname: 图像名称，其中"s01_"表示该图像是基于受试者 01 的问卷答案生成的

#Exp2_Dataratings_Full.csv: 包含了所有参与者的完整响应集，用于评估"Exp2_Reliabiliy.R"中的测试-重测得分

#DarawithImgQuest.csv: 仅包含 SR 图像的数据，并添加了一个"Question"列以指示图像是从哪个问题生成，与"Exp2_QuestionClass.R"分析一起使用




