// index.js
const app = getApp()

Page({
  data: {
    searchInput: '',
    searchHistory: [],
    showHistory: false
  },
  
  onLoad: function() {
    // 从本地存储加载搜索历史
    const history = wx.getStorageSync('searchHistory') || []
    this.setData({
      searchHistory: history
    })
  },
  
  // 输入框内容变化时触发
  onInputChange: function(e) {
    this.setData({
      searchInput: e.detail.value,
      showHistory: e.detail.value === ''
    })
  },
  
  // 点击搜索按钮或回车时触发
  onSearch: function() {
    const code = this.data.searchInput.trim()
    if (!code) {
      wx.showToast({
        title: '请输入公司代码或名称',
        icon: 'none'
      })
      return
    }
    
    // 保存到搜索历史
    this.saveSearchHistory(code)
    
    // 跳转到结果页
    wx.navigateTo({
      url: `/pages/result/result?code=${code}`
    })
  },
  
  // 点击历史记录项
  onHistoryItemTap: function(e) {
    const code = e.currentTarget.dataset.code
    this.setData({
      searchInput: code
    })
    
    // 跳转到结果页
    wx.navigateTo({
      url: `/pages/result/result?code=${code}`
    })
  },
  
  // 清空历史记录
  clearHistory: function() {
    wx.showModal({
      title: '提示',
      content: '确定要清空搜索历史吗？',
      success: (res) => {
        if (res.confirm) {
          this.setData({
            searchHistory: []
          })
          wx.removeStorageSync('searchHistory')
        }
      }
    })
  },
  
  // 保存搜索历史
  saveSearchHistory: function(code) {
    let history = this.data.searchHistory
    
    // 如果已存在，则移到最前面
    const index = history.indexOf(code)
    if (index > -1) {
      history.splice(index, 1)
    }
    
    // 添加到最前面
    history.unshift(code)
    
    // 最多保存10条
    if (history.length > 10) {
      history = history.slice(0, 10)
    }
    
    this.setData({
      searchHistory: history
    })
    
    // 保存到本地存储
    wx.setStorageSync('searchHistory', history)
  }
})