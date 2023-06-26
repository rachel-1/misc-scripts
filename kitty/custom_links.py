import re

def mark(text, args, Mark, extra_cli_args, *a):
    # This function is responsible for finding all
    # matching text. extra_cli_args are any extra arguments
    # passed on the command line when invoking the kitten.
    # We mark all individual word for potential selection

    # The following are intended matches. Test with https://regex101.com/ with the flags gms
    # 1.
    # ^[]8;;https://mlab.common.cloud.aurora.tech/app/reports/59205^[\http
    # s://mlab.common.cloud.aurora.tech/app/reports/59205^[]8;;^[\$ 
    
    for idx, m in enumerate(re.finditer(r'^.*\s*?(https?:\/\/[^\^\$]*).*\$', text, re.MULTILINE | re.DOTALL)):
        start, end = m.span(1)
        mark_text = text[start:end].replace('\\', '').replace('\n', '')
        # The empty dictionary below will be available as groupdicts
        # in handle_result() and can contain arbitrary data.
        yield Mark(idx, start, end, mark_text, {})


def handle_result(args, data, target_window_id, boss, extra_cli_args, *a):
    matches, groupdicts = [], []
    for m, g in zip(data['match'], data['groupdicts']):
        if m:
            matches.append(m), groupdicts.append(g)
    for link, match_data in zip(matches, groupdicts):
        w = boss.window_id_map.get(target_window_id)
        if w is not None:
            w.open_url(link, hyperlink_id=1)
