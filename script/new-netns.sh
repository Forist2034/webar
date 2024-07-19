#!/bin/sh

set -o errexit -o xtrace -o nounset

readonly ns=$1
readonly out_iface=$2
readonly veth_ns="ve-${1}-ns"
readonly veth_host="ve-${1}-host"

ip netns add $ns
ip link add $veth_ns type veth peer name $veth_host
ip link set $veth_ns netns $ns
ip -n $ns addr add 10.100.1.2/24 dev $veth_ns
ip addr add 10.100.1.1/24 dev $veth_host
ip -n $ns link set $veth_ns up
ip link set $veth_host up
ip -n $ns route add default via 10.100.1.1
iptables -t nat -A POSTROUTING -s 10.100.1.0/24 -o $out_iface -j MASQUERADE
sysctl -w net.ipv4.ip_forward=1
