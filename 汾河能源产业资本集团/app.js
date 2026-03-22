// app.js
App({
  onLaunch: function () {
    console.log('小程序启动')
  },
  globalData: {
    // 全局数据
    companyData: require('./data/companies.js')
  }
})