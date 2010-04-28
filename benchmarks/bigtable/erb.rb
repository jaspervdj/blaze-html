# BigTable benchmark implemented in ERB.
#
require 'erb'
require 'benchmark'
include ERB::Util

table = (1 .. 1000).map do |_| (1 .. 10) end

template = ERB.new <<-EOF
<table>
  <% table.each do |row| %>
    <tr>
      <% row.each do |value| %>
        <td>
            <%=h value %>
        </td>
      <% end %>
    </tr>
  <% end %>
</table>
EOF

puts Benchmark.measure { 100.times do |_| template.result(binding) end }
