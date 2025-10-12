#!/usr/bin/env python3
"""
ast_diff_viewer.py - Visual AST comparison tool for round-trip validation
Generates side-by-side comparison and highlights structural differences
"""

import json
import sys
from pathlib import Path
from difflib import SequenceMatcher
from typing import Dict, List, Tuple, Any

class ASTNode:
    def __init__(self, node_type: str, data: Dict):
        self.node_type = node_type
        self.data = data
        self.children = []
        
    def __repr__(self):
        return f"{self.node_type}({self.data.get('name', 'unnamed')})"

def parse_json_tree(json_data: Dict) -> ASTNode:
    """Convert JSON tree to simplified AST structure"""
    node_type = json_data.get('type', 'UNKNOWN')
    node = ASTNode(node_type, json_data)
    
    # Recursively parse children based on common patterns
    for key in json_data:
        if key.endswith('sp'):  # Common Verilator pattern for statement/expression lists
            value = json_data[key]
            if isinstance(value, list):
                for child_data in value:
                    if isinstance(child_data, dict):
                        node.children.append(parse_json_tree(child_data))
    
    return node

def extract_module_info(node: ASTNode, modules: List[Dict] = None) -> List[Dict]:
    """Extract module-level information"""
    if modules is None:
        modules = []
    
    if node.node_type in ['MODULE', 'NETLIST']:
        mod_info = {
            'name': node.data.get('name', 'unnamed'),
            'type': node.node_type,
            'statement_count': len(node.children),
            'statements': [child.node_type for child in node.children]
        }
        modules.append(mod_info)
    
    for child in node.children:
        extract_module_info(child, modules)
    
    return modules

def flatten_tree(node: ASTNode, path: str = "", depth: int = 0) -> List[Tuple[str, str, int]]:
    """Flatten tree to list of (path, type, depth) tuples for comparison"""
    result = [(path, node.node_type, depth)]
    
    for i, child in enumerate(node.children):
        child_path = f"{path}/{node.node_type}[{i}]"
        result.extend(flatten_tree(child, child_path, depth + 1))
    
    return result

def compare_structures(original: List[Tuple], roundtrip: List[Tuple]) -> Dict:
    """Compare two flattened tree structures"""
    orig_types = [t for _, t, _ in original]
    rt_types = [t for _, t, _ in roundtrip]
    
    matcher = SequenceMatcher(None, orig_types, rt_types)
    similarity = matcher.ratio()
    
    differences = []
    for tag, i1, i2, j1, j2 in matcher.get_opcodes():
        if tag == 'replace':
            differences.append({
                'type': 'replace',
                'original': original[i1:i2],
                'roundtrip': roundtrip[j1:j2]
            })
        elif tag == 'delete':
            differences.append({
                'type': 'delete',
                'original': original[i1:i2],
                'roundtrip': []
            })
        elif tag == 'insert':
            differences.append({
                'type': 'insert',
                'original': [],
                'roundtrip': roundtrip[j1:j2]
            })
    
    return {
        'similarity': similarity,
        'differences': differences,
        'original_node_count': len(original),
        'roundtrip_node_count': len(roundtrip)
    }

def print_tree(node: ASTNode, indent: int = 0, max_depth: int = 5):
    """Pretty print tree structure"""
    if indent > max_depth:
        return
    
    prefix = "  " * indent
    name = node.data.get('name', '')
    if name:
        print(f"{prefix}├─ {node.node_type} ({name})")
    else:
        print(f"{prefix}├─ {node.node_type}")
    
    for child in node.children[:10]:  # Limit to first 10 children
        print_tree(child, indent + 1, max_depth)
    
    if len(node.children) > 10:
        print(f"{prefix}  ... and {len(node.children) - 10} more children")

def print_comparison_report(orig_file: str, rt_file: str, 
                           orig_tree: ASTNode, rt_tree: ASTNode):
    """Print detailed comparison report"""
    
    print("\n" + "="*70)
    print("AST Round-Trip Comparison Report")
    print("="*70)
    print(f"Original:   {orig_file}")
    print(f"Round-trip: {rt_file}")
    print()
    
    # Module-level comparison
    print("Module-Level Analysis")
    print("-" * 70)
    
    orig_modules = extract_module_info(orig_tree)
    rt_modules = extract_module_info(rt_tree)
    
    print(f"Original modules:   {len(orig_modules)}")
    print(f"Round-trip modules: {len(rt_modules)}")
    
    if len(orig_modules) != len(rt_modules):
        print(f"⚠️  MODULE COUNT MISMATCH!")
    
    # Compare each module
    for i, (orig_mod, rt_mod) in enumerate(zip(orig_modules, rt_modules)):
        print(f"\nModule {i+1}:")
        print(f"  Original:   {orig_mod['name']} ({orig_mod['statement_count']} statements)")
        print(f"  Round-trip: {rt_mod['name']} ({rt_mod['statement_count']} statements)")
        
        if orig_mod['name'] != rt_mod['name']:
            print(f"  ❌ Name mismatch!")
        
        if orig_mod['statement_count'] != rt_mod['statement_count']:
            print(f"  ⚠️  Statement count differs: {orig_mod['statement_count']} vs {rt_mod['statement_count']}")
    
    print()
    print("Structural Analysis")
    print("-" * 70)
    
    # Flatten and compare structures
    orig_flat = flatten_tree(orig_tree)
    rt_flat = flatten_tree(rt_tree)
    
    comparison = compare_structures(orig_flat, rt_flat)
    
    print(f"Similarity: {comparison['similarity']*100:.1f}%")
    print(f"Original nodes:   {comparison['original_node_count']}")
    print(f"Round-trip nodes: {comparison['roundtrip_node_count']}")
    print(f"Differences:      {len(comparison['differences'])}")
    
    if comparison['differences']:
        print("\n" + "Detailed Differences (first 20)")
        print("-" * 70)
        
        for i, diff in enumerate(comparison['differences'][:20], 1):
            print(f"\nDifference {i} ({diff['type']}):")
            
            if diff['type'] == 'replace':
                print("  Original:")
                for path, node_type, depth in diff['original'][:5]:
                    print(f"    {'  '*depth}{node_type}")
                print("  Round-trip:")
                for path, node_type, depth in diff['roundtrip'][:5]:
                    print(f"    {'  '*depth}{node_type}")
            
            elif diff['type'] == 'delete':
                print("  Deleted from original:")
                for path, node_type, depth in diff['original'][:5]:
                    print(f"    {'  '*depth}{node_type}")
            
            elif diff['type'] == 'insert':
                print("  Inserted in round-trip:")
                for path, node_type, depth in diff['roundtrip'][:5]:
                    print(f"    {'  '*depth}{node_type}")
    
    # Statement-level statistics
    print()
    print("Statement Type Distribution")
    print("-" * 70)
    
    orig_types = {}
    for _, node_type, _ in orig_flat:
        orig_types[node_type] = orig_types.get(node_type, 0) + 1
    
    rt_types = {}
    for _, node_type, _ in rt_flat:
        rt_types[node_type] = rt_types.get(node_type, 0) + 1
    
    all_types = sorted(set(orig_types.keys()) | set(rt_types.keys()))
    
    print(f"{'Type':<20} {'Original':>10} {'Round-trip':>10} {'Diff':>10}")
    print("-" * 52)
    
    for node_type in all_types[:20]:  # Show top 20
        orig_count = orig_types.get(node_type, 0)
        rt_count = rt_types.get(node_type, 0)
        diff = rt_count - orig_count
        
        diff_str = f"+{diff}" if diff > 0 else str(diff)
        if diff != 0:
            print(f"{node_type:<20} {orig_count:>10} {rt_count:>10} {diff_str:>10} ⚠️")
        else:
            print(f"{node_type:<20} {orig_count:>10} {rt_count:>10} {diff_str:>10} ✓")
    
    if len(all_types) > 20:
        print(f"\n... and {len(all_types) - 20} more types")
    
    print("\n" + "="*70)
    
    # Return summary for batch processing
    return {
        'similarity': comparison['similarity'],
        'differences': len(comparison['differences']),
        'module_match': len(orig_modules) == len(rt_modules),
        'node_count_diff': comparison['roundtrip_node_count'] - comparison['original_node_count']
    }

def print_tree_view(orig_file: str, rt_file: str, 
                    orig_tree: ASTNode, rt_tree: ASTNode,
                    max_depth: int = 4):
    """Print side-by-side tree view"""
    
    print("\n" + "="*70)
    print("Tree Structure View (limited to depth {})".format(max_depth))
    print("="*70)
    
    print("\nOriginal Tree:")
    print("-" * 70)
    print_tree(orig_tree, max_depth=max_depth)
    
    print("\nRound-trip Tree:")
    print("-" * 70)
    print_tree(rt_tree, max_depth=max_depth)
    print()

def compare_files(orig_file: str, rt_file: str, show_tree: bool = False):
    """Compare two JSON tree files"""
    
    try:
        with open(orig_file, 'r') as f:
            orig_json = json.load(f)
        
        with open(rt_file, 'r') as f:
            rt_json = json.load(f)
    
    except FileNotFoundError as e:
        print(f"Error: File not found - {e}")
        return None
    except json.JSONDecodeError as e:
        print(f"Error: Invalid JSON - {e}")
        return None
    
    # Parse trees
    orig_tree = parse_json_tree(orig_json)
    rt_tree = parse_json_tree(rt_json)
    
    # Show tree view if requested
    if show_tree:
        print_tree_view(orig_file, rt_file, orig_tree, rt_tree)
    
    # Print comparison report
    return print_comparison_report(orig_file, rt_file, orig_tree, rt_tree)

def batch_compare(orig_dir: str, rt_dir: str, output_file: str = None):
    """Compare all files in two directories"""
    
    orig_path = Path(orig_dir)
    rt_path = Path(rt_dir)
    
    orig_files = list(orig_path.glob("*.json"))
    
    print(f"Found {len(orig_files)} JSON files to compare\n")
    
    results = []
    
    for orig_file in orig_files:
        rt_file = rt_path / orig_file.name
        
        if not rt_file.exists():
            print(f"⚠️  Skipping {orig_file.name} - no round-trip file")
            continue
        
        print(f"Comparing: {orig_file.name}")
        result = compare_files(str(orig_file), str(rt_file))
        
        if result:
            results.append({
                'filename': orig_file.name,
                **result
            })
    
    # Summary
    print("\n" + "="*70)
    print("Batch Comparison Summary")
    print("="*70)
    
    if results:
        perfect = sum(1 for r in results if r['differences'] == 0)
        avg_sim = sum(r['similarity'] for r in results) / len(results)
        
        print(f"Files compared: {len(results)}")
        print(f"Perfect matches: {perfect} ({perfect/len(results)*100:.1f}%)")
        print(f"Average similarity: {avg_sim*100:.1f}%")
        
        print("\nFiles with most differences:")
        sorted_results = sorted(results, key=lambda x: x['differences'], reverse=True)
        for r in sorted_results[:10]:
            status = "✓" if r['differences'] == 0 else "✗"
            print(f"  {status} {r['filename']:<40} Sim: {r['similarity']*100:>5.1f}% Diffs: {r['differences']:>3}")
        
        # Save to JSON if requested
        if output_file:
            with open(output_file, 'w') as f:
                json.dump({
                    'summary': {
                        'total': len(results),
                        'perfect': perfect,
                        'avg_similarity': avg_sim
                    },
                    'results': results
                }, f, indent=2)
            print(f"\nResults saved to: {output_file}")

def main():
    if len(sys.argv) < 3:
        print("AST Round-Trip Comparison Tool")
        print("\nUsage:")
        print("  Single file:  {} <original.json> <roundtrip.json> [--tree]".format(sys.argv[0]))
        print("  Batch:        {} <original_dir> <roundtrip_dir> [output.json]".format(sys.argv[0]))
        print("\nOptions:")
        print("  --tree        Show tree structure view (single file mode)")
        print("\nExample:")
        print("  {} obj_dir/module.json roundtrip_json/module.json --tree".format(sys.argv[0]))
        print("  {} obj_dir/ roundtrip_json/ comparison_results.json".format(sys.argv[0]))
        sys.exit(1)
    
    arg1 = sys.argv[1]
    arg2 = sys.argv[2]
    
    if Path(arg1).is_file() and Path(arg2).is_file():
        # Single file mode
        show_tree = '--tree' in sys.argv
        compare_files(arg1, arg2, show_tree)
    
    elif Path(arg1).is_dir() and Path(arg2).is_dir():
        # Batch mode
        output_file = sys.argv[3] if len(sys.argv) > 3 else None
        batch_compare(arg1, arg2, output_file)
    
    else:
        print("Error: Invalid arguments")
        print("Both arguments must be files or both must be directories")
        sys.exit(1)

if __name__ == '__main__':
    main()
