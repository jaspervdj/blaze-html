# BigTable benchmark implemented in erubis
#
require 'erubis'
require 'benchmark'

table = (1 .. 1000).map do |_| (1 .. 10) end

template = Erubis::Eruby.new <<-EOF
<table>
  <% table.each do |row| %>
    <tr>
      <% row.each do |value| %>
        <td>
            <%== value %>
        </td>
      <% end %>
    </tr>
  <% end %>
</table>
EOF

puts Benchmark.measure { 100.times do |_| template.result(binding) end }
