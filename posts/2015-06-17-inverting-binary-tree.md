---
title: Inverting a Binary Tree
author: Austin
---

Recently, the creator of Homebrew, [Max Howell](https://github.com/mxcl) interviewed at Google, got a question involving inverting a binary tree, and 


```ruby
class TreeNode
  attr_accessor :left, :right, :val
  def initialize(val)
    @val = val
  end
end

def preorder(node)
  return [] if node.nil?
  [].tap do |temp|
    temp << node.val
    temp.concat(preorder(node.left))
    temp.concat(preorder(node.right))
  end
end

def invert_tree(root, left_child, right_child)
  node_count = left_child.size
  nodes = []
  (1..node_count - 1).each do |i|
     nodes << TreeNode.new(i)
  end
  nodes.each_with_index do |node, index|
    node.left = nodes[right_child[index+1]-1] if right_child[index+1] > 0
    node.right = nodes[left_child[index+1]-1] if left_child[index+1] > 0
  end
  root_node = nodes[root-1]

  node_list = preorder(root_node)
  puts node_list.join(' ')
end


invert_tree(3, [0, 0, 0, 2, 1, 0], [0, 0, 0, 4, 5, 0])
```

http://www.jasq.org/just-another-scala-quant/inverting-binary-trees-considered-harmful
