namespace StuckOnDayTwelve;

public class Program
{
    public static void Main()
    {
        Console.WriteLine(Directory.GetCurrentDirectory());
        var lines = File.ReadAllLines("../../input/day12.txt");
        Console.WriteLine($"Part one: {PartOne(lines)}");
        Console.WriteLine($"Part two: {PartTwo(lines)}");
    }

    private static long PartOne(string[] lines)
    {
        var counts = lines
            .Select(line => ConditionRecord.Parse(line, 1))
            .Select(rec => CountMatches(rec.Springs.AsSpan(), rec.Groups))
            .ToArray();
        return counts.Sum();
    }

    private static long PartTwo(string[] lines)
    {
        var counts = lines
            .Select(line => ConditionRecord.Parse(line, 5))
            .Select((rec, index) =>
            {
                var matches = CountMatches(rec.Springs.AsSpan(), rec.Groups);
                Console.WriteLine($"Line {index + 1}, {matches} arrangement(s)");
                return matches;
            })
            .ToArray();
        return counts.Sum();
    }

    private static long CountMatches(ReadOnlySpan<Condition> template, int[] groups)
    {
        if (template.Length == 0)
            return groups.Length > 0 ? 0L : 1L;
        
        if (groups.Length == 0)
        {
            foreach (var condition in template)
            {
                if (condition == Condition.Damaged)
                    return 0L;
            }

            return 1L;
        }
        
        var largestIndex = GetLargestIndex(groups);
        var largestGroup = groups[largestIndex];
        
        var leftGroups = groups[..largestIndex];
        var rightGroups = groups[(largestIndex + 1)..];

        var leftBuffer = leftGroups.Sum() + leftGroups.Length;
        var rightBuffer = rightGroups.Sum() + rightGroups.Length;
        
        var count = 0L;

        var stop = template.Length - rightBuffer - largestGroup;
        for (var start = leftBuffer; start <= stop; start++)
        {
            var end = start + largestGroup;
            var sub = template[start..end];

            if (!NoneOperational(sub))
                continue;

            if (start > 0 && template[start - 1] == Condition.Damaged)
                continue;
            
            if (end < template.Length && template[end] == Condition.Damaged)
                continue;
            
            var leftSprings = template[..(start > 0 ? start - 1 : 0)];
            var rightSprings = template[(end < (template.Length - 1) ? end + 1 : end)..];
            var leftCount = CountMatches(leftSprings, leftGroups);
            var rightCount = CountMatches(rightSprings, rightGroups);
            count += leftCount * rightCount;
        }
        
        return count;
    }

    private static bool CanFit(ReadOnlySpan<Condition> springs, int length)
    {
        if (springs.Length == length)
            return NoneOperational(springs);

        var isBuffer = springs[length] != Condition.Damaged;
        return isBuffer && NoneOperational(springs[..length]);
    }

    private static bool NoneOperational(ReadOnlySpan<Condition> springs)
    {
        foreach (var spring in springs)
        {
            if (spring == Condition.Operational)
                return false;
        }
    
        return true;
    }
    
    private static int GetLargestIndex(int[] groups)
    {
        var largest = 0;
        var index = 0;

        for (var i = 0; i < groups.Length; i++)
        {
            if (groups[i] > largest)
            {
                largest = groups[i];
                index = i;
            }
        }

        return index;
    }

    private static bool AreSpringsValid(Condition[] truth, Condition[] conditions)
    {
        if (truth.Length != conditions.Length)
            return false;

        for (var i = 0; i < truth.Length; i++)
        {
            if (truth[i] == Condition.Damaged && conditions[i] != Condition.Damaged)
                return false;

            if (truth[i] == Condition.Operational && conditions[i] != Condition.Operational)
                return false;
        }
        
        return true;
    }

    private static bool AreGroupsValid(int[] truth, Condition[] springs)
    {
        var groups = new List<int>();
        var counter = 0;

        foreach (var spring in springs)
        {
            if (spring == Condition.Damaged)
            {
                counter++;
            }
            else if (spring == Condition.Operational && counter > 0)
            {
                groups.Add(counter);
                counter = 0;
            }
        }

        if (groups.Count != truth.Length)
            return false;

        for (var i = 0; i < truth.Length; i++)
        {
            if (truth[i] != groups[i])
                return false;
        }

        return true;
    }
}

enum Condition : byte
{
    Operational,
    Damaged,
    Unknown
}

class ConditionRecord
{
    public required Condition[] Springs;
    public required int[] Groups;

    public static ConditionRecord Parse(string line, int repeat)
    {
        var parts = line.Split(' ');
        
        var springs = Repeat(parts[0], repeat, '?')
            .Select(c =>
            {
                return c switch
                {
                    '.' => Condition.Operational,
                    '#' => Condition.Damaged,
                    _ => Condition.Unknown
                };
            }).ToArray();

        var groups = Repeat(parts[1], repeat, ',')
            .Split(',').Select(int.Parse).ToArray();

        return new ConditionRecord
        {
            Springs = springs,
            Groups = groups
        };
    }
    
    private static string Repeat(string s, int times, char connector)
    {
        return string.Join(connector, Enumerable.Repeat(s, times));
    }
}
