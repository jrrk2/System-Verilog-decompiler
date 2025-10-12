#!/usr/bin/env python3
"""
stream_quality_analyzer.py - Memory-efficient quality analyzer for large files
Processes files line-by-line to avoid loading entire files into memory
"""

import sys
import os
import re
from pathlib import Path
from collections import defaultdict
import json

class QualityMetrics:
    def __init__(self, filename):
        self.filename = filename
        self.line_count = 0
        self.total_chars = 0
        self.max_line_length = 0
        
        # Delimiter tracking
        self.open_paren = 0
        self.close_paren = 0
        self.open_brace = 0
        self.close_brace = 0
        self.open_bracket = 0
        self.close_bracket = 0
        
        # Structure tracking
        self.module_count = 0
        self.endmodule_count = 0
        self.always_blocks = 0
        self.assign_count = 0
        
        # Port tracking
        self.input_ports = 0
        self.output_ports = 0
        self.inout_ports = 0
        
        # Issue tracking
        self.debug_vars = 0
        self.unknown_refs = 0
        self.json_fragments = 0
        self.empty_blocks = 0
        self.double_semicolons = 0
        self.very_long_lines = []
        
        # Keyword tracking
        self.sv_keywords = defaultdict(int)
        
    def process_line(self, line_num, line):
        """Process a single line and update metrics"""
        self.line_count += 1
        line_len = len(line)
        self.total_chars += line_len
        self.max_line_length = max(self.max_line_length, line_len)
        
        if line_len > 1000:
            self.very_long_lines.append((line_num, line_len))
        
        # Count delimiters
        self.open_paren += line.count('(')
        self.close_paren += line.count(')')
        self.open_brace += line.count('{')
        self.close_brace += line.count('}')
        self.open_bracket += line.count('[')
        self.close_bracket += line.count(']')
        
        # Check for structure keywords
        if re.match(r'^module\s+', line):
            self.module_count += 1
        if re.match(r'^endmodule', line):
            self.endmodule_count += 1
        if 'always_' in line or 'always @' in line:
            self.always_blocks += 1
        if line.strip().startswith('assign '):
            self.assign_count += 1
            
        # Check for ports
        if re.match(r'\s*input\s+', line):
            self.input_ports += 1
        if re.match(r'\s*output\s+', line):
            self.output_ports += 1
        if re.match(r'\s*inout\s+', line):
            self.inout_ports += 1
            
        # Check for issues
        if '__Vdfg' in line:
            self.debug_vars += 1
        if 'UNKNOWN' in line:
            self.unknown_refs += 1
        if '"type":' in line:
            self.json_fragments += 1
        if re.search(r'begin\s*end', line):
            self.empty_blocks += 1
        if ';;' in line:
            self.double_semicolons += 1
            
        # Track SystemVerilog keywords
        keywords = ['logic', 'always_comb', 'always_ff', 'typedef', 
                   'struct', 'enum', 'interface', 'modport']
        for kw in keywords:
            if kw in line:
                self.sv_keywords[kw] += 1
    
    def calculate_scores(self):
        """Calculate quality scores"""
        scores = {}
        
        # Delimiter balance (0-1)
        paren_balanced = self.open_paren == self.close_paren
        brace_balanced = self.open_brace == self.close_brace
        bracket_balanced = self.open_bracket == self.close_bracket
        scores['delimiter_balance'] = 1.0 if (paren_balanced and brace_balanced and bracket_balanced) else 0.0
        
        # Module structure (0-1)
        scores['module_structure'] = 1.0 if (self.module_count > 0 and self.module_count == self.endmodule_count) else 0.0
        
        # Port declarations (0-1)
        total_ports = self.input_ports + self.output_ports + self.inout_ports
        scores['has_ports'] = 1.0 if total_ports > 0 else 0.0
        
        # No artifacts (0-1)
        artifacts = self.debug_vars + self.unknown_refs + self.json_fragments
        if artifacts == 0:
            scores['no_artifacts'] = 1.0
        elif artifacts < 10:
            scores['no_artifacts'] = 0.5
        else:
            scores['no_artifacts'] = 0.0
            
        # No common errors (0-1)
        errors = self.empty_blocks + self.double_semicolons + len(self.very_long_lines)
        if errors == 0:
            scores['no_errors'] = 1.0
        elif errors < 5:
            scores['no_errors'] = 0.7
        else:
            scores['no_errors'] = 0.3
            
        # SystemVerilog features (0-1)
        sv_feature_count = len([k for k, v in self.sv_keywords.items() if v > 0])
        scores['sv_features'] = min(1.0, sv_feature_count / 4.0)
        
        # Overall score
        scores['overall'] = sum(scores.values()) / len(scores)
        
        return scores
    
    def generate_report(self):
        """Generate a detailed report"""
        scores = self.calculate_scores()
        
        report = {
            'filename': os.path.basename(self.filename),
            'stats': {
                'lines': self.line_count,
                'total_chars': self.total_chars,
                'max_line_length': self.max_line_length,
                'avg_line_length': self.total_chars / self.line_count if self.line_count > 0 else 0,
            },
            'structure': {
                'modules': self.module_count,
                'endmodules': self.endmodule_count,
                'always_blocks': self.always_blocks,
                'assigns': self.assign_count,
            },
            'ports': {
                'inputs': self.input_ports,
                'outputs': self.output_ports,
                'inouts': self.inout_ports,
                'total': self.input_ports + self.output_ports + self.inout_ports,
            },
            'delimiters': {
                'parens': {'open': self.open_paren, 'close': self.close_paren, 'balanced': self.open_paren == self.close_paren},
                'braces': {'open': self.open_brace, 'close': self.close_brace, 'balanced': self.open_brace == self.close_brace},
                'brackets': {'open': self.open_bracket, 'close': self.close_bracket, 'balanced': self.open_bracket == self.close_bracket},
            },
            'issues': {
                'debug_vars': self.debug_vars,
                'unknown_refs': self.unknown_refs,
                'json_fragments': self.json_fragments,
                'empty_blocks': self.empty_blocks,
                'double_semicolons': self.double_semicolons,
                'very_long_lines': len(self.very_long_lines),
            },
            'sv_keywords': dict(self.sv_keywords),
            'scores': scores,
        }
        
        return report

def analyze_file(filepath):
    """Analyze a single file line by line"""
    metrics = QualityMetrics(filepath)
    
    try:
        with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
            for line_num, line in enumerate(f, 1):
                metrics.process_line(line_num, line)
    except Exception as e:
        print(f"Error reading {filepath}: {e}", file=sys.stderr)
        return None
    
    return metrics.generate_report()

def print_report(report):
    """Pretty print a single report"""
    print("\n" + "="*60)
    print(f"Quality Report: {report['filename']}")
    print("="*60)
    
    scores = report['scores']
    print(f"\n📊 Overall Quality Score: {scores['overall']*100:.1f}%")
    
    print("\n📈 Individual Scores:")
    for key, value in scores.items():
        if key != 'overall':
            status = "✓" if value >= 0.8 else "⚠" if value >= 0.5 else "✗"
            print(f"  {status} {key.replace('_', ' ').title()}: {value*100:.1f}%")
    
    print("\n📝 File Statistics:")
    stats = report['stats']
    print(f"  Lines: {stats['lines']:,}")
    print(f"  Characters: {stats['total_chars']:,}")
    print(f"  Max line length: {stats['max_line_length']}")
    print(f"  Avg line length: {stats['avg_line_length']:.1f}")
    
    print("\n🏗️  Structure:")
    struct = report['structure']
    print(f"  Modules: {struct['modules']} (endmodules: {struct['endmodules']})")
    print(f"  Always blocks: {struct['always_blocks']}")
    print(f"  Assign statements: {struct['assigns']}")
    
    print("\n🔌 Ports:")
    ports = report['ports']
    print(f"  Inputs: {ports['inputs']}")
    print(f"  Outputs: {ports['outputs']}")
    print(f"  Total: {ports['total']}")
    
    print("\n⚖️  Delimiters:")
    delims = report['delimiters']
    for name, info in delims.items():
        status = "✓" if info['balanced'] else "✗"
        print(f"  {status} {name.title()}: {info['open']} open, {info['close']} close")
    
    issues = report['issues']
    if any(issues.values()):
        print("\n⚠️  Issues Found:")
        if issues['debug_vars'] > 0:
            print(f"  • Debug variables: {issues['debug_vars']}")
        if issues['unknown_refs'] > 0:
            print(f"  • Unknown references: {issues['unknown_refs']}")
        if issues['json_fragments'] > 0:
            print(f"  • JSON fragments: {issues['json_fragments']}")
        if issues['empty_blocks'] > 0:
            print(f"  • Empty blocks: {issues['empty_blocks']}")
        if issues['double_semicolons'] > 0:
            print(f"  • Double semicolons: {issues['double_semicolons']}")
        if issues['very_long_lines'] > 0:
            print(f"  • Lines >1000 chars: {issues['very_long_lines']}")
    else:
        print("\n✓ No issues found!")
    
    if report['sv_keywords']:
        print("\n🔧 SystemVerilog Features:")
        for kw, count in sorted(report['sv_keywords'].items(), key=lambda x: x[1], reverse=True):
            if count > 0:
                print(f"  • {kw}: {count}")

def analyze_directory(dir_path, output_json=None, output_csv=None):
    """Analyze all .sv files in a directory"""
    dir_path = Path(dir_path)
    sv_files = list(dir_path.glob('**/*.sv'))
    
    if not sv_files:
        print(f"No .sv files found in {dir_path}")
        return
    
    print(f"Found {len(sv_files)} SystemVerilog files to analyze\n")
    
    reports = []
    for sv_file in sv_files:
        print(f"Analyzing: {sv_file.name}...", end=' ')
        report = analyze_file(sv_file)
        if report:
            reports.append(report)
            score = report['scores']['overall']
            status = "✓" if score >= 0.8 else "⚠" if score >= 0.5 else "✗"
            print(f"{status} {score*100:.1f}%")
        else:
            print("✗ Failed")
    
    # Print individual reports
    for report in reports:
        print_report(report)
    
    # Print summary
    print("\n" + "="*60)
    print("Summary Statistics")
    print("="*60)
    
    if reports:
        avg_score = sum(r['scores']['overall'] for r in reports) / len(reports)
        passed = sum(1 for r in reports if r['scores']['overall'] >= 0.8)
        warned = sum(1 for r in reports if 0.5 <= r['scores']['overall'] < 0.8)
        failed = sum(1 for r in reports if r['scores']['overall'] < 0.5)
        
        print(f"\nFiles analyzed: {len(reports)}")
        print(f"Average quality score: {avg_score*100:.1f}%")
        print(f"\n✓ Passed (≥80%): {passed}")
        print(f"⚠ Warning (50-79%): {warned}")
        print(f"✗ Failed (<50%): {failed}")
        
        # Find common issues
        total_issues = defaultdict(int)
        for report in reports:
            for issue, count in report['issues'].items():
                if count > 0:
                    total_issues[issue] += 1
        
        if total_issues:
            print("\n📋 Common Issues Across Files:")
            for issue, file_count in sorted(total_issues.items(), key=lambda x: x[1], reverse=True):
                pct = (file_count / len(reports)) * 100
                print(f"  • {issue.replace('_', ' ').title()}: {file_count} files ({pct:.1f}%)")
    
    # Save JSON report
    if output_json:
        with open(output_json, 'w') as f:
            json.dump({
                'summary': {
                    'total_files': len(reports),
                    'avg_score': avg_score,
                    'passed': passed,
                    'warned': warned,
                    'failed': failed,
                },
                'reports': reports
            }, f, indent=2)
        print(f"\n💾 JSON report saved to: {output_json}")
    
    # Save CSV summary
    if output_csv:
        with open(output_csv, 'w') as f:
            f.write("Filename,Overall Score,Delimiter Balance,Module Structure,Has Ports,No Artifacts,No Errors,SV Features,Lines,Ports,Issues\n")
            for report in reports:
                scores = report['scores']
                stats = report['stats']
                ports = report['ports']
                issues_count = sum(report['issues'].values())
                
                f.write(f"{report['filename']},{scores['overall']:.3f},")
                f.write(f"{scores['delimiter_balance']:.3f},")
                f.write(f"{scores['module_structure']:.3f},")
                f.write(f"{scores['has_ports']:.3f},")
                f.write(f"{scores['no_artifacts']:.3f},")
                f.write(f"{scores['no_errors']:.3f},")
                f.write(f"{scores['sv_features']:.3f},")
                f.write(f"{stats['lines']},")
                f.write(f"{ports['total']},")
                f.write(f"{issues_count}\n")
        print(f"📊 CSV summary saved to: {output_csv}")

def main():
    if len(sys.argv) < 2:
        print("Usage:")
        print(f"  Single file:  {sys.argv[0]} <file.sv>")
        print(f"  Directory:    {sys.argv[0]} <directory> [output.json] [output.csv]")
        print("\nExample:")
        print(f"  {sys.argv[0]} results/ quality_report.json quality_summary.csv")
        sys.exit(1)
    
    target = sys.argv[1]
    
    if os.path.isfile(target):
        # Single file mode
        report = analyze_file(target)
        if report:
            print_report(report)
        else:
            print(f"Failed to analyze {target}")
            sys.exit(1)
    elif os.path.isdir(target):
        # Directory mode
        output_json = sys.argv[2] if len(sys.argv) > 2 else None
        output_csv = sys.argv[3] if len(sys.argv) > 3 else None
        analyze_directory(target, output_json, output_csv)
    else:
        print(f"Error: {target} is not a valid file or directory")
        sys.exit(1)

if __name__ == '__main__':
    main()