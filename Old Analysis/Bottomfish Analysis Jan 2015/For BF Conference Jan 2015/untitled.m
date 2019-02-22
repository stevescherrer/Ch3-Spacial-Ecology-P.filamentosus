%Trying to fix dates.

rd = ReceiverDates;
bad_index = find(isnan(Bottom.Fish(:,4) ));
bad_receivers = Bottom.Fish(bad_index, 2);
bottomfish = Bottom.Fish;
bf_bad = bottomfish(bad_index, :);
[v,i] = sort(bf_bad(:,2));
fix = [];
for a = 2:length(bf_bad(i,1))
    if bf_bad(a,3) ~= bf_bad(a-1, 3);
        fix = [fix; bf_bad(a-1,2), bf_bad(a-1,1); bf_bad(a,2), bf_bad(a,1) ];
    end
end