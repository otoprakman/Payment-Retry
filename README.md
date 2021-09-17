# Improving Payment Recycling Practices

## Introduction

You have heard a lot about new streaming platform X from your friends and decided to give it a try and signed up. You filled all your personal and billing information. You chose a monthly plan which also gives an option to cancel anytime. Now you are ready to enjoy your time at X.

On X side, they have millions of subscribers across the globe. They are always looking ways for better customer experience which includes the content, platform and each other touch points. With your subscription, they got a new subscribed user and they need to collect fees for your membership which is crucial to continue their business. It is called recurring payment which is an online payment that repeats according to a preset schedule. 

There are a lot of different payment processors that can help X to accept payments such as: Paypal, Square, Stripe etc. What is important in here is that after X set up monthly payments, there is a possibility that authorization attempt may fail. There might be several reasons behind that including insufficient fund, fraud, expired card, gateway issues etc. In addition to those reasons related to the billing information, there might be another external factors related to the card issuing bank or card network. Considering the periodic transactions of millions of global customers, small increase in approval rate will lead high revenue and positive customer experience. Conversely, failure in payments will adversely affect customer experience, resulting in lower revenue and involuntary churn.

After X observes a failure in charging, they can request card update from the card scheme, repeat authorization attempt after some time, try other methods or cancel the subscription which is undesired for the business. In this post we are going to interest in retrying method and seek improvements in retry logic.

Most of payment processors make authorization attempt at preset time and if it fails retry next attempt in a fixed time window and for fixed number of times. This rule based approach doesn’t consider historical data and may cause redundant retry attempts. 

We can partition the problem into two sub-problem; predicting the likelihood of successful initial billing and recurring billing. In the first problem we assume that the merchant is going to charge the initial billing and we want to find the best time to charge. In the second problem, we assume that initial billing failed and we want to retry next attempt at the best time.

(Please consider that this blog post is not written by a Subject-matter expert. All shared information is based on the resources mentioned at Resources section and author’s personal experience. Main purpose of this post is to share an idea regarding how to solve well known payment analytic problem.)

Lets assume that our data have;
- Timestamp of the transaction
- Payment instrument metadata: It can be contextual card information like card network, card type etc.
- Response codes:  Response codes generated by customer’s bank that indicates why transaction is approved or declined. “Do not honor” is a very common response that could mean anything other than a specific reason.
- Transaction: Binary variable that represents if the transaction is declined or approved.
It includes both new sign-ups and recurring attempts which comprises around %10 of all transactions.

As an exploratory data analysis, we can take a look at time related decline ratio.

<img src="https://render.githubusercontent.com/render/math?math=Decline\ Ratio = \frac {\sum{Successful\ Authorization\ Attempts}} {\sum{Authorization\ Attempts}}">

Decline ratio can be defined as above. Note that both recycling and single attempts are considered in the formula

![hour_day](https://user-images.githubusercontent.com/53580699/133819745-9ef91e3a-5db1-4817-9680-ebb682580836.png)

Here we observe that there are high decline ratio at early hours in a day. One of the reason could be it is unlikely to transaction happen so it could be seen as suspicious.

![Rplot](https://user-images.githubusercontent.com/53580699/133821494-7f67ad33-62a5-4cc7-ace4-06b0397f5ca9.png)

In this plot, we observe that there is difference between days of a week in terms of the decline ratio. Proportion of transactions for each day are displayed on each point.

![Rplot01](https://user-images.githubusercontent.com/53580699/133825824-c14bc2c0-2b40-49d7-be09-0319b8c33b2a.png)


## Resources
1) https://medium.com/disney-streaming/building-a-rule-engine-that-helps-to-optimize-recurring-churn-failures-at-scale-in-disney-a6b41bc6614b
2) https://recurly.com/blog/what-are-recurring-payments-and-subscription-billing/
3) https://recurly.com/blog/how-data-science-work-reveals-hidden-trends-in-payment-success-rates/
4) https://recurly.com/blog/the-real-meaning-of-churn/
5) https://docs.recurly.com/docs/retry-logic
6) https://stripe.com/en-au/guides/optimizing-authorization-rates
