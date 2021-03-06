# tock

*tock*是一个目录生成工具。当前仅支持生成markdown目录。

## 用法

```
Usage: tock dir [-x]
  Generate TOC

Available options:
  dir                      The root directory to look for contents
  -x                       Exclude directories that do not have files
                           (recursively)
  -h,--help                Show this help text
```

## 示例

比如某文件夹memos内容如下：

```
memos
├── 软件工程
│   ├── 方法论、典范和实践
│   │   └── 极限编程.md
│   └── 软件测试方法和形式验证.md
├── 音乐
│   └── 尤克里里弹唱谱
│       ├── 对不起我爱你.txt
│       ├── 天亮了.txt
│       ├── 相思.txt
│       ├── Butterfly<和田光司>.txt
│       └── Dream Solister.txt
├── Haskell
│   ├── 异常处理最佳实践.md
│   └── RecursionSchemes.md
└── Linux
    └── Fontconfig解析逻辑.md
```

执行：

```bash
$ tock -x memos
```

行到如下结果：

```markdown
* [Haskell](memos%2FHaskell)
  * [RecursionSchemes.md](memos%2FHaskell%2FRecursionSchemes.md)
  * [异常处理最佳实践.md](memos%2FHaskell%2F%E5%BC%82%E5%B8%B8%E5%A4%84%E7%90%86%E6%9C%80%E4%BD%B3%E5%AE%9E%E8%B7%B5.md)
* [Linux](memos%2FLinux)
  * [Fontconfig解析逻辑.md](memos%2FLinux%2FFontconfig%E8%A7%A3%E6%9E%90%E9%80%BB%E8%BE%91.md)
* [软件工程](memos%2F%E8%BD%AF%E4%BB%B6%E5%B7%A5%E7%A8%8B)
  * [软件测试方法和形式验证.md](memos%2F%E8%BD%AF%E4%BB%B6%E5%B7%A5%E7%A8%8B%2F%E8%BD%AF%E4%BB%B6%E6%B5%8B%E8%AF%95%E6%96%B9%E6%B3%95%E5%92%8C%E5%BD%A2%E5%BC%8F%E9%AA%8C%E8%AF%81.md)
  * [方法论、典范和实践](memos%2F%E8%BD%AF%E4%BB%B6%E5%B7%A5%E7%A8%8B%2F%E6%96%B9%E6%B3%95%E8%AE%BA%E3%80%81%E5%85%B8%E8%8C%83%E5%92%8C%E5%AE%9E%E8%B7%B5)
    * [极限编程.md](memos%2F%E8%BD%AF%E4%BB%B6%E5%B7%A5%E7%A8%8B%2F%E6%96%B9%E6%B3%95%E8%AE%BA%E3%80%81%E5%85%B8%E8%8C%83%E5%92%8C%E5%AE%9E%E8%B7%B5%2F%E6%9E%81%E9%99%90%E7%BC%96%E7%A8%8B.md)
* [音乐](memos%2F%E9%9F%B3%E4%B9%90)
  * [尤克里里弹唱谱](memos%2F%E9%9F%B3%E4%B9%90%2F%E5%B0%A4%E5%85%8B%E9%87%8C%E9%87%8C%E5%BC%B9%E5%94%B1%E8%B0%B1)
    * [Butterfly<和田光司>.txt](memos%2F%E9%9F%B3%E4%B9%90%2F%E5%B0%A4%E5%85%8B%E9%87%8C%E9%87%8C%E5%BC%B9%E5%94%B1%E8%B0%B1%2FButterfly%3C%E5%92%8C%E7%94%B0%E5%85%89%E5%8F%B8%3E.txt)
    * [Dream Solister.txt](memos%2F%E9%9F%B3%E4%B9%90%2F%E5%B0%A4%E5%85%8B%E9%87%8C%E9%87%8C%E5%BC%B9%E5%94%B1%E8%B0%B1%2FDream%20Solister.txt)
    * [天亮了.txt](memos%2F%E9%9F%B3%E4%B9%90%2F%E5%B0%A4%E5%85%8B%E9%87%8C%E9%87%8C%E5%BC%B9%E5%94%B1%E8%B0%B1%2F%E5%A4%A9%E4%BA%AE%E4%BA%86.txt)
    * [对不起我爱你.txt](memos%2F%E9%9F%B3%E4%B9%90%2F%E5%B0%A4%E5%85%8B%E9%87%8C%E9%87%8C%E5%BC%B9%E5%94%B1%E8%B0%B1%2F%E5%AF%B9%E4%B8%8D%E8%B5%B7%E6%88%91%E7%88%B1%E4%BD%A0.txt)
    * [相思.txt](memos%2F%E9%9F%B3%E4%B9%90%2F%E5%B0%A4%E5%85%8B%E9%87%8C%E9%87%8C%E5%BC%B9%E5%94%B1%E8%B0%B1%2F%E7%9B%B8%E6%80%9D.txt)
```

展示效果如下：

* [Haskell](memos%2FHaskell)
  * [RecursionSchemes.md](memos%2FHaskell%2FRecursionSchemes.md)
  * [异常处理最佳实践.md](memos%2FHaskell%2F%E5%BC%82%E5%B8%B8%E5%A4%84%E7%90%86%E6%9C%80%E4%BD%B3%E5%AE%9E%E8%B7%B5.md)
* [Linux](memos%2FLinux)
  * [Fontconfig解析逻辑.md](memos%2FLinux%2FFontconfig%E8%A7%A3%E6%9E%90%E9%80%BB%E8%BE%91.md)
* [软件工程](memos%2F%E8%BD%AF%E4%BB%B6%E5%B7%A5%E7%A8%8B)
  * [软件测试方法和形式验证.md](memos%2F%E8%BD%AF%E4%BB%B6%E5%B7%A5%E7%A8%8B%2F%E8%BD%AF%E4%BB%B6%E6%B5%8B%E8%AF%95%E6%96%B9%E6%B3%95%E5%92%8C%E5%BD%A2%E5%BC%8F%E9%AA%8C%E8%AF%81.md)
  * [方法论、典范和实践](memos%2F%E8%BD%AF%E4%BB%B6%E5%B7%A5%E7%A8%8B%2F%E6%96%B9%E6%B3%95%E8%AE%BA%E3%80%81%E5%85%B8%E8%8C%83%E5%92%8C%E5%AE%9E%E8%B7%B5)
    * [极限编程.md](memos%2F%E8%BD%AF%E4%BB%B6%E5%B7%A5%E7%A8%8B%2F%E6%96%B9%E6%B3%95%E8%AE%BA%E3%80%81%E5%85%B8%E8%8C%83%E5%92%8C%E5%AE%9E%E8%B7%B5%2F%E6%9E%81%E9%99%90%E7%BC%96%E7%A8%8B.md)
* [音乐](memos%2F%E9%9F%B3%E4%B9%90)
  * [尤克里里弹唱谱](memos%2F%E9%9F%B3%E4%B9%90%2F%E5%B0%A4%E5%85%8B%E9%87%8C%E9%87%8C%E5%BC%B9%E5%94%B1%E8%B0%B1)
    * [Butterfly<和田光司>.txt](memos%2F%E9%9F%B3%E4%B9%90%2F%E5%B0%A4%E5%85%8B%E9%87%8C%E9%87%8C%E5%BC%B9%E5%94%B1%E8%B0%B1%2FButterfly%3C%E5%92%8C%E7%94%B0%E5%85%89%E5%8F%B8%3E.txt)
    * [Dream Solister.txt](memos%2F%E9%9F%B3%E4%B9%90%2F%E5%B0%A4%E5%85%8B%E9%87%8C%E9%87%8C%E5%BC%B9%E5%94%B1%E8%B0%B1%2FDream%20Solister.txt)
    * [天亮了.txt](memos%2F%E9%9F%B3%E4%B9%90%2F%E5%B0%A4%E5%85%8B%E9%87%8C%E9%87%8C%E5%BC%B9%E5%94%B1%E8%B0%B1%2F%E5%A4%A9%E4%BA%AE%E4%BA%86.txt)
    * [对不起我爱你.txt](memos%2F%E9%9F%B3%E4%B9%90%2F%E5%B0%A4%E5%85%8B%E9%87%8C%E9%87%8C%E5%BC%B9%E5%94%B1%E8%B0%B1%2F%E5%AF%B9%E4%B8%8D%E8%B5%B7%E6%88%91%E7%88%B1%E4%BD%A0.txt)
    * [相思.txt](memos%2F%E9%9F%B3%E4%B9%90%2F%E5%B0%A4%E5%85%8B%E9%87%8C%E9%87%8C%E5%BC%B9%E5%94%B1%E8%B0%B1%2F%E7%9B%B8%E6%80%9D.txt)
