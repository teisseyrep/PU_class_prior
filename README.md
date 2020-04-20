# PU_class_prior


Learning from positive and unlabelled data (PU learning) has attracted increasing interest within the machine learning literature as this type of data naturally arises in many applications (under reporting, text classification, disease gene identification). In the case of PU data, we have an access to positive examples and unlabeled examples. Unlabeled examples can be either positive or negative. In this setting the true class label Y (0,1) is not observed directly. We only observe surrogate variable S, which indicates whether an example is labeled (and thus positive; S =1) or unlabeled (S=0). 
It is known that in positive unlabelled setting, a classifier can be successfully learned if the class prior is available. In this project we consider a novel method of class prior estimation which is based on logistic regression. The proposed approach involves simultaneous estimation of label frequency and model parameters. In order to account for the non-concavity of the likelihood function,  we proposed a novel optimization procedure, called CD+MM, which is a combination of  cyclic coordinate descent and  Minorization-Maximization algorithms.

See test.R file to learn how to use the proposed method.

This is a joint project with Małgorzata Łazęcka (Polish Academy of Sciences) and Jan Mielniczuk (Polish Academy of Sciences)
