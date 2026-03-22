// result.js
const app = getApp()

Page({
  data: {
    company: null,
    loading: true,
    error: null
  },
  
  onLoad: function(options) {
    const { code } = options
    
    if (!code) {
      this.setData({
        loading: false,
        error: '未提供公司代码'
      })
      return
    }
    
    // 获取公司数据
    this.fetchCompanyData(code)
  },
  
  fetchCompanyData: function(code) {
    // 从全局数据中获取公司信息
    const companyData = app.globalData.companyData
    
    // 模拟网络请求延迟
    setTimeout(() => {
      const company = companyData[code]
      
      if (company) {
        this.setData({
          company,
          loading: false
        })
      } else {
        // 如果找不到公司数据
        this.setData({
          loading: false,
          error: '未找到该公司信息'
        })
      }
    }, 500)
  },
  
  // 返回首页
  goBack: function() {
    wx.navigateBack({
      delta: 1
    })
  },
  
  // 分享功能
  onShareAppMessage: function() {
    const company = this.data.company
    return {
      title: company ? `${company.name}的企业信息` : '上市公司知识库',
      path: company ? `/pages/result/result?code=${company.code}` : '/pages/index/index'
    }
  }
})